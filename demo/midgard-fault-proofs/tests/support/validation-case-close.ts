/**
 * The closes on a `CaseWorld`: a timed-out thread into its case, and the
 * case itself with its bond. Each builder returns the transaction before
 * signing, so a test can evaluate a changed one and see it refused.
 */
import {
  Case,
  CaseRedeemer,
  DisputeBondSpendRedeemer,
  LostDefenseReceipt,
  ReceiptRedeemer,
  requireInputIndex,
  ValidationTimeoutSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  caseUnit,
  computationThreadUnit,
  fraudProofUnit,
  type OpenCase,
} from "./validation-case-moves.js";
import { threadTokenName } from "./validation-case-names.js";
import {
  type CaseWorld,
  referenceOf,
  submitSigned,
  treasuryAddress,
} from "./validation-case-scene.js";
import { gameRedeemer } from "./validation-game-moves.js";
import {
  cap,
  createdAt,
  fraudProofOutputLovelace,
  keyAddress,
  moveWindow,
  tagOf,
  type Thread,
  threadUnit,
} from "./validation-game-scene.js";

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);
const bigMax = (a: bigint, b: bigint) => (a > b ? a : b);
const caseRedeemer = (redeemer: CaseRedeemer) =>
  Data.to(redeemer, CaseRedeemer);

/** What a convicted close leaves beside the locked case NFT and marker. */
export const lockLovelace = 1_500_000n;

export type Outcome = "DefenderWins" | "ChallengerWins";

/** Who wins a thread whose side to move has run out of clock. */
export const timeoutOutcome = (thread: Thread): Outcome =>
  thread.dispute.turn.type === "awaitingOperator"
    ? "ChallengerWins"
    : "DefenderWins";

/**
 * The close of a thread at the timeout validator into its case. With
 * `lone`, the case and the thread are the only inputs and the thread pays
 * the fee, `fee`; otherwise a wallet input pays it.
 */
export const buildCloseTimedOutThread = (
  world: CaseWorld,
  opened: OpenCase,
  thread: Thread,
  {
    lone = false,
    fee = 0n,
    outcome = timeoutOutcome(thread),
  }: {
    readonly lone?: boolean;
    readonly fee?: bigint;
    readonly outcome?: Outcome;
  } = {},
) => {
  const defenderVkh = thread.identity.defenderVkh;
  const lost = outcome !== "DefenderWins";
  const receiptUnit =
    world.award.hash + threadTokenName(opened.caseId, defenderVkh);
  const receiptLovelace = lost ? fraudProofOutputLovelace : 0n;
  const datum: Case = {
    ...opened.datum,
    open_threads: opened.datum.open_threads - 1n,
    defender_won: opened.datum.defender_won || !lost,
  };
  const caseClose = (ctx: Parameters<typeof requireInputIndex>[0]) =>
    caseRedeemer({
      ThreadClose: {
        case_output_index: 0n,
        thread_input_index: requireInputIndex(ctx, thread.utxo, "thread"),
        outcome,
        defender_vkh: defenderVkh,
      },
    });
  let tx = world.lucid
    .newTx()
    .collectFrom([thread.utxo], (ctx) =>
      Data.to(
        {
          input_index: requireInputIndex(ctx, thread.utxo, "thread"),
          case_input_index: requireInputIndex(ctx, opened.utxo, "case"),
        },
        ValidationTimeoutSpendRedeemer,
      ),
    )
    .collectFrom([opened.utxo], caseClose)
    .mintAssets({ [threadUnit(world, thread.identity)]: -1n }, caseClose)
    .readFrom([
      world.config,
      referenceOf(world, world.caseValidator),
      referenceOf(world, world.timeout),
      ...(lost ? [referenceOf(world, world.award)] : []),
    ])
    .pay.ToContract(
      world.caseValidator.address,
      { kind: "inline", value: Data.to(datum, Case) },
      opened.utxo.assets,
    );
  if (lost) {
    tx = tx
      .mintAssets({ [receiptUnit]: 1n }, (ctx) =>
        Data.to(
          {
            MintReceipt: {
              case_input_index: requireInputIndex(ctx, opened.utxo, "case"),
            },
          },
          ReceiptRedeemer,
        ),
      )
      .pay.ToContract(
        world.award.address,
        {
          kind: "inline",
          value: Data.to(
            {
              case_id: opened.caseId,
              defender_vkh: defenderVkh,
              outcome,
              bounty_claimant: opened.datum.bounty_claimant,
            },
            LostDefenseReceipt,
          ),
        },
        { lovelace: receiptLovelace, [receiptUnit]: 1n },
      );
  }
  const window = moveWindow(world.emulator.now());
  tx = tx.pay
    .ToAddressWithData(
      keyAddress(lost ? opened.datum.bounty_claimant : defenderVkh),
      { kind: "inline", value: tagOf(thread.utxo) },
      { lovelace: lovelaceOf(thread.utxo) - receiptLovelace - fee },
    )
    .validFrom(window.validFrom)
    .validTo(window.validTo);
  const built: Promise<TxSignBuilder> = lone
    ? tx.setMinFee(fee).complete({ coinSelection: false, localUPLCEval: true })
    : tx.complete({ localUPLCEval: true });
  return {
    built,
    receiptUnit,
    next: async (hash: string) => ({
      ...opened,
      utxo: await createdAt(world, hash, 0),
      datum,
    }),
  };
};

export const closeTimedOutThread = async (
  world: CaseWorld,
  opened: OpenCase,
  thread: Thread,
) => {
  const { built, next } = buildCloseTimedOutThread(world, opened, thread);
  const { hash, budget } = await submitSigned(world, await built);
  return { opened: await next(hash), hash, budget };
};

/** `validation_case_v1.case_outcome`. */
export const caseOutcome = (datum: Case) =>
  datum.defender_won || datum.open_threads > 0n
    ? "DefenderWon"
    : datum.neither_claim_valid
      ? "NoValidClaim"
      : "Convicted";

/**
 * The case close, now: the bond back to the challenger on a conviction
 * (beside the fraud-proof token and the locked case tokens), to the
 * treasury otherwise. `bond` replaces the case's own bond input, and
 * `bondTo` the address its output pays.
 */
export const buildCloseCase = (
  world: CaseWorld,
  opened: OpenCase,
  {
    bond = opened.bond,
    bondTo,
  }: { readonly bond?: UTxO; readonly bondTo?: string } = {},
) => {
  const convicted = caseOutcome(opened.datum) === "Convicted";
  const claimant = keyAddress(opened.datum.bounty_claimant);
  const close = (ctx: Parameters<typeof requireInputIndex>[0]) =>
    caseRedeemer({
      CaseClose: {
        bond_input_index: requireInputIndex(ctx, bond, "bond"),
        bond_output_index: 0n,
        remainder_output_index: 1n,
      },
    });
  let tx = world.lucid
    .newTx()
    .collectFrom([opened.utxo], close)
    .collectFrom([bond], (ctx) =>
      Data.to(
        { case_input_index: requireInputIndex(ctx, opened.utxo, "case") },
        DisputeBondSpendRedeemer,
      ),
    )
    .mintAssets({ [computationThreadUnit(world, opened)]: -1n })
    .attach.MintingPolicy(world.computationThreadPolicy.script)
    .readFrom([
      world.config,
      ...(convicted ? [world.hub] : []),
      referenceOf(world, world.caseValidator),
      referenceOf(world, world.bond),
    ])
    .pay.ToAddressWithData(
      bondTo ?? (convicted ? claimant : treasuryAddress()),
      { kind: "inline", value: tagOf(bond) },
      { lovelace: lovelaceOf(bond) },
    );
  const own = lovelaceOf(opened.utxo);
  // The case-policy tokens the bond input carries: the case's marker on its
  // own bond.
  const markers = Object.fromEntries(
    Object.entries(bond.assets).filter(([unit]) =>
      unit.startsWith(world.caseValidator.hash),
    ),
  );
  if (convicted) {
    tx = tx.pay
      .ToAddressWithData(
        claimant,
        { kind: "inline", value: tagOf(opened.utxo) },
        {
          lovelace: bigMax(
            own - fraudProofOutputLovelace - lockLovelace,
            1_500_000n,
          ),
        },
      )
      .mintAssets({ [fraudProofUnit(world, opened)]: 1n })
      .attach.MintingPolicy(world.fraudProofPolicy.script)
      .pay.ToContract(
        world.fraudProofAddress,
        {
          kind: "inline",
          value: Data.to(new Constr(0, [opened.datum.bounty_claimant])),
        },
        {
          lovelace: fraudProofOutputLovelace,
          [fraudProofUnit(world, opened)]: 1n,
        },
      )
      .pay.ToAddress(world.fraudProofAddress, {
        lovelace: lockLovelace,
        [caseUnit(world, opened)]: 1n,
        ...markers,
      });
  } else {
    tx = tx
      .mintAssets(
        {
          [caseUnit(world, opened)]: -1n,
          ...Object.fromEntries(
            Object.keys(markers).map((unit) => [unit, -1n]),
          ),
        },
        close,
      )
      .pay.ToAddressWithData(
        claimant,
        { kind: "inline", value: tagOf(opened.utxo) },
        { lovelace: own },
      );
  }
  const window = moveWindow(world.emulator.now());
  return tx
    .validFrom(window.validFrom)
    .validTo(window.validTo)
    .complete({ localUPLCEval: true });
};

export const closeCase = async (world: CaseWorld, opened: OpenCase) =>
  submitSigned(world, await buildCloseCase(world, opened));

/**
 * The expiry of a live thread at the game, which its case can no longer
 * take: the game releases it to the thread expiry withdrawal, the case
 * policy burns its token, and the thread alone pays the capped fee, the rest
 * going to its defender.
 */
export const buildExpireThread = (world: CaseWorld, thread: Thread) => {
  const window = moveWindow(world.emulator.now());
  const references = [
    world.game.hash,
    world.caseValidator.hash,
    world.threadExpiry.hash,
  ];
  return world.lucid
    .newTx()
    .collectFrom([thread.utxo], (ctx) =>
      gameRedeemer({ kind: "EnterChallengerTimeout" })({
        input_index: requireInputIndex(ctx, thread.utxo, "thread"),
        output_index: 0n,
      }),
    )
    .withdraw(world.threadExpiry.reward, 0n, Data.void())
    .mintAssets(
      { [threadUnit(world, thread.identity)]: -1n },
      Data.to("BurnThreadToken", CaseRedeemer),
    )
    .readFrom([
      world.config,
      ...references.map((hash) => {
        const reference = world.scriptReferences.get(hash);
        if (reference === undefined) throw new Error("unpublished script");
        return reference;
      }),
    ])
    .pay.ToAddressWithData(
      keyAddress(thread.identity.defenderVkh),
      { kind: "inline", value: tagOf(thread.utxo) },
      { lovelace: lovelaceOf(thread.utxo) - cap },
    )
    .validFrom(window.validFrom)
    .validTo(window.validTo)
    .setMinFee(cap)
    .complete({ coinSelection: false, localUPLCEval: true });
};
