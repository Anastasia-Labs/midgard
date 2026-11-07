/**
 * The case transactions on a `CaseWorld`: VerifySource (through the source
 * seam), Defend, the close of a timed-out thread into its case, and the case
 * close. Each builder returns the transaction before signing, so a test can
 * evaluate a changed one and see it refused.
 */
import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  Case,
  CaseRedeemer,
  castActiveOperatorDatumToData,
  DisputeBond,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  LinkedListDatum,
  Proof,
  requireInputIndex,
  validationMachineStateDataFromCore,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { Constr, Data, fromText, type UTxO } from "@lucid-evolution/lucid";

import { type CompleteSignedTransactionMeasurement } from "./submit-init-emulator-shared.js";
import { bondMarkerName, caseIdOf } from "./validation-case-names.js";
import {
  bondLovelace,
  caseFloorLovelace,
  type CaseWorld,
  defenseBondUnlockTime,
  referenceOf,
  sideClockMs,
  submitSigned,
  threadFloorLovelace,
} from "./validation-case-scene.js";
import {
  type DeepGameTraces,
  type SparseTrace,
} from "./validation-game-deep-traces.js";
import {
  createdAt,
  type DeployedScript,
  moveWindow,
  newKey,
  type Thread,
  threadDatum,
  threadUnit,
} from "./validation-game-scene.js";
import { openGame } from "./validation-game-traces.js";

const hex = (bytes: Buffer) => bytes.toString("hex");

export type OpenCase = {
  readonly utxo: UTxO;
  readonly datum: Case;
  readonly caseId: string;
  readonly bond: UTxO;
  readonly traces: DeepGameTraces;
  /** The computation-thread token name; the fraud-proof token's too. */
  readonly computationThreadName: string;
  /** Key hashes of the operators that have defended, in order. */
  readonly defenders: readonly string[];
};

export const caseUnit = (world: CaseWorld, opened: OpenCase) =>
  world.caseValidator.hash + opened.caseId;
export const markerUnit = (world: CaseWorld, opened: OpenCase) =>
  world.caseValidator.hash + bondMarkerName(opened.caseId);
export const computationThreadUnit = (world: CaseWorld, opened: OpenCase) =>
  world.computationThreadPolicy.hash + opened.computationThreadName;
export const fraudProofUnit = (world: CaseWorld, opened: OpenCase) =>
  world.fraudProofPolicy.hash + opened.computationThreadName;

const caseRedeemer = (redeemer: CaseRedeemer) =>
  Data.to(redeemer, CaseRedeemer);

/**
 * VerifySource through the seam: a case over `traces`, its challenger the
 * world's prover, and its bond. An `opener` of the wallet's own stands for
 * an input of any other script.
 */
export const buildOpenCase = async (
  world: CaseWorld,
  traces: DeepGameTraces,
  {
    headerHash = "aa".repeat(28),
    opener = "source",
  }: {
    readonly headerHash?: string;
    readonly opener?: "source" | "wallet";
  } = {},
) => {
  const openerUtxo = await createdAt(
    world,
    (
      await submitSigned(
        world,
        await world.lucid
          .newTx()
          .pay.ToAddress(
            opener === "source" ? world.source.address : world.wallet.address,
            { lovelace: 5_000_000n },
          )
          .complete({ localUPLCEval: true }),
      )
    ).hash,
    0,
  );
  const eventKey: EventKey = {
    L2TransactionEventKey: { tx_id: "ee".repeat(32) },
  };
  const caseId = caseIdOf(headerHash, Data.to(eventKey, EventKey), openerUtxo);
  const window = moveWindow(world.emulator.now());
  const datum: Case = {
    version: 1n,
    block_header_hash: headerHash,
    event_key: eventKey,
    challenger_root: hex(traces.challenger.descriptor.traceRoot),
    step_count: BigInt(traces.stepCount),
    challenger_terminal_hash: hex(
      traces.challenger.descriptor.terminalStateHash,
    ),
    bounty_claimant: world.proverKey,
    agreed_initial_hash: hex(traces.operator.descriptor.initialStateHash),
    started_at: BigInt(window.currentTimeUpper),
    open_threads: 0n,
    defenders_root: EMPTY_MERKLE_TREE_ROOT,
    defender_won: false,
    neither_claim_valid: false,
  };
  const opened = {
    datum,
    caseId,
    traces,
    computationThreadName: `00000006${headerHash}`,
    defenders: [],
  };
  const units = {
    case: world.caseValidator.hash + caseId,
    marker: world.caseValidator.hash + bondMarkerName(caseId),
    computationThread:
      world.computationThreadPolicy.hash + opened.computationThreadName,
  };
  let tx = world.lucid.newTx().collectFrom([openerUtxo]);
  if (opener === "source")
    tx = tx.attach.SpendingValidator(world.source.script);
  tx = tx
    .mintAssets({ [units.case]: 1n, [units.marker]: 1n }, (ctx) =>
      caseRedeemer({
        VerifySource: {
          opener_input_index: requireInputIndex(ctx, openerUtxo, "opener"),
          case_output_index: 0n,
        },
      }),
    )
    .mintAssets({ [units.computationThread]: 1n })
    .attach.MintingPolicy(world.computationThreadPolicy.script)
    .readFrom([world.config, referenceOf(world, world.caseValidator)])
    .pay.ToContract(
      world.caseValidator.address,
      { kind: "inline", value: Data.to(datum, Case) },
      {
        lovelace: caseFloorLovelace,
        [units.case]: 1n,
        [units.computationThread]: 1n,
      },
    )
    .pay.ToContract(
      world.bond.address,
      {
        kind: "inline",
        value: Data.to(
          {
            challenged_header_hash: headerHash,
            fraud_prover: world.proverKey,
            case_id: caseId,
          },
          DisputeBond,
        ),
      },
      { lovelace: bondLovelace, [units.marker]: 1n },
    )
    .validFrom(window.validFrom)
    .validTo(window.validTo);
  return { opened, built: tx.complete({ localUPLCEval: true }) };
};

export const openCase = async (
  world: CaseWorld,
  traces: DeepGameTraces,
  options: { readonly headerHash?: string } = {},
): Promise<
  OpenCase & { readonly budget: CompleteSignedTransactionMeasurement }
> => {
  const { opened, built } = await buildOpenCase(world, traces, options);
  const { hash, budget } = await submitSigned(world, await built);
  return {
    ...opened,
    budget,
    utxo: await createdAt(world, hash, 0),
    bond: await createdAt(world, hash, 1),
  };
};

export type Defender = {
  readonly signingKey: string;
  readonly vkh: string;
  readonly node: UTxO;
};

export const nodeDatum = (bondUnlockTime: number | null) =>
  Data.to(
    {
      data: {
        Node: {
          data: castActiveOperatorDatumToData({
            bond_unlock_time:
              bondUnlockTime === null ? null : BigInt(bondUnlockTime),
            inactivity_strikes: 0n,
          }) as Data,
        },
      },
      link: null,
    },
    LinkedListDatum,
  );

/** The bond hold an operator node carries. */
export const bondUnlockTimeOf = (node: UTxO): number | null => {
  const datum = Data.from(node.datum!, LinkedListDatum);
  if (!("Node" in datum.data)) throw new Error("not an operator node");
  const fields = (datum.data.Node.data as Constr<Data>).fields;
  const hold = fields[0] as Constr<Data>;
  return hold.index === 0 ? Number(hold.fields[0] as bigint) : null;
};

/**
 * An operator's node at the node holder, its token under `policy`: by
 * default the active-operators policy the hub names.
 */
export const registerDefender = async (
  world: CaseWorld,
  bondUnlockTime: number | null = null,
  policy: DeployedScript = world.activeOperatorsPolicy,
): Promise<Defender> => {
  const key = newKey();
  const unit = policy.hash + fromText("MACT") + key.hash;
  const { hash } = await submitSigned(
    world,
    await world.lucid
      .newTx()
      .mintAssets({ [unit]: 1n })
      .attach.MintingPolicy(policy.script)
      .pay.ToContract(
        world.nodeHolder.address,
        { kind: "inline", value: nodeDatum(bondUnlockTime) },
        { lovelace: 5_000_000n, [unit]: 1n },
      )
      .complete({ localUPLCEval: true }),
  );
  return {
    signingKey: key.signingKey,
    vkh: key.hash,
    node: await createdAt(world, hash, 0),
  };
};

const defendersTrie = async (defenders: readonly string[]) => {
  const trie = new Trie();
  for (const vkh of defenders) {
    await trie.insert(Buffer.from(vkh, "hex"), Buffer.from(vkh, "hex"));
  }
  return trie;
};

export type DefenseChanges = {
  /** The bond hold the node output carries; by default the honest one. */
  readonly bondUnlockTime?: number;
  /** How many thread tokens the transaction mints; by default one. */
  readonly threadTokens?: 0 | 1 | 2;
  readonly signed?: boolean;
  /** The thread's lovelace; by default the floor. */
  readonly threadLovelace?: bigint;
  /** The defender's claimed trace; by default the operator trace. */
  readonly claim?: SparseTrace;
};

/** Defend by `defender`, now; the defender's clock is charged since the open. */
export const buildDefend = async (
  world: CaseWorld,
  opened: OpenCase,
  defender: Defender,
  changes: DefenseChanges = {},
) => {
  const window = moveWindow(world.emulator.now());
  const startedAt = Number(opened.datum.started_at);
  const stepCount = opened.traces.stepCount;
  const claim = changes.claim ?? opened.traces.operatorTrace;
  const dispute = {
    ...openGame(opened.traces, window.currentTimeUpper),
    operatorDescriptor: claim.descriptor,
    operatorHighHash: claim.proofAt(stepCount).stateHash,
    operatorClockMs: sideClockMs - Math.max(0, window.validFrom - startedAt),
  };
  const identity = {
    caseId: opened.caseId,
    startedAt,
    defenderVkh: defender.vkh,
  };
  const trie = await defendersTrie(opened.defenders);
  const proof = Data.from(
    (await trie.prove(Buffer.from(defender.vkh, "hex"), true))
      .toCBOR()
      .toString("hex"),
    Proof,
  );
  if (!opened.defenders.includes(defender.vkh)) {
    await trie.insert(
      Buffer.from(defender.vkh, "hex"),
      Buffer.from(defender.vkh, "hex"),
    );
  }
  const datum: Case = {
    ...opened.datum,
    open_threads: opened.datum.open_threads + 1n,
    defenders_root: hex(trie.hash!),
  };
  const oldHold = bondUnlockTimeOf(defender.node);
  const needed = defenseBondUnlockTime(startedAt);
  const hold =
    changes.bondUnlockTime ??
    (oldHold === null ? needed : Math.max(oldHold, needed));
  const unit = threadUnit(world, identity);
  const tokens = changes.threadTokens ?? 1;
  const redeemer = (ctx: Parameters<typeof requireInputIndex>[0]) =>
    caseRedeemer({
      Defend: {
        case_output_index: 0n,
        thread_output_index: 1n,
        active_node_input_index: requireInputIndex(ctx, defender.node, "node"),
        defender_vkh: defender.vkh,
        defenders_proof: proof,
        defender_root: hex(claim.descriptor.traceRoot),
        defender_state: validationMachineStateDataFromCore(
          claim.stateAt(stepCount),
        ),
        defender_state_proof: validationTraceProofDataFromCore(
          claim.proofAt(stepCount),
        ),
      },
    });
  let tx = world.lucid
    .newTx()
    .collectFrom([opened.utxo], redeemer)
    .collectFrom([defender.node])
    .attach.SpendingValidator(world.nodeHolder.script);
  if (tokens === 2) {
    const second = threadUnit(world, {
      ...identity,
      defenderVkh: "ef".repeat(28),
    });
    tx = tx.mintAssets({ [unit]: 1n, [second]: 1n }, redeemer);
  } else if (tokens === 1) {
    tx = tx.mintAssets({ [unit]: 1n }, redeemer);
  }
  tx = tx
    .readFrom([
      world.config,
      world.hub,
      referenceOf(world, world.caseValidator),
    ])
    .pay.ToContract(
      world.caseValidator.address,
      { kind: "inline", value: Data.to(datum, Case) },
      opened.utxo.assets,
    )
    .pay.ToContract(
      world.game.address,
      { kind: "inline", value: threadDatum(world, identity, dispute) },
      {
        lovelace: changes.threadLovelace ?? threadFloorLovelace,
        ...(tokens === 0 ? {} : { [unit]: 1n }),
      },
    )
    .pay.ToContract(
      world.nodeHolder.address,
      { kind: "inline", value: nodeDatum(hold) },
      defender.node.assets,
    )
    .validFrom(window.validFrom)
    .validTo(window.validTo);
  if (changes.signed ?? true) tx = tx.addSignerKey(defender.vkh);
  return {
    built: tx.complete({ localUPLCEval: true }),
    next: (hash: string) =>
      Promise.all([
        createdAt(world, hash, 0),
        createdAt(world, hash, 1),
        createdAt(world, hash, 2),
      ]).then(([caseUtxo, threadUtxo, node]) => ({
        opened: {
          ...opened,
          utxo: caseUtxo,
          datum,
          defenders: [...opened.defenders, defender.vkh],
        },
        thread: { utxo: threadUtxo, dispute, identity } satisfies Thread,
        defender: { ...defender, node },
      })),
  };
};

export const defend = async (
  world: CaseWorld,
  opened: OpenCase,
  defender: Defender,
) => {
  const { built, next } = await buildDefend(world, opened, defender);
  const { hash, budget } = await submitSigned(world, await built, [
    defender.signingKey,
  ]);
  return { ...(await next(hash)), budget };
};
