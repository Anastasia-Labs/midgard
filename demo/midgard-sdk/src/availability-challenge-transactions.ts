import {
  type Assets,
  type BuildTxWithRedeemer,
  calculateMinLovelaceFromUTxO,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type Script,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as Availability from "./availability-challenge.js";
import { scriptRewardAddress } from "./cardano-addresses.js";
import { type MidgardValidators, outputReferenceFromUTxO } from "./common.js";
import {
  CorrectionLockDatum,
  CorrectionLockRedeemer,
  correctionLockUnit,
} from "./correction-lock.js";
import { HUB_ORACLE_ASSET_NAME } from "./hub-oracle.js";
import { castStateQueueNodeToData, StateQueueNode } from "./ledger-state.js";
import {
  encodeLinkedListNodeView,
  type LinkedListNodeView,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { referenceScriptAuthUnit } from "./reference-scripts.js";
import {
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  StateQueueSpendRedeemer,
  type StateQueueUTxO,
  utxoToStateQueueUTxO,
} from "./state-queue.js";
import { completeOptionsWithLocalEval } from "./tx-completion.js";
import {
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";
import {
  isPlainPositiveAdaOnlyUtxo,
  outputDatumCborMatches,
} from "./tx-output-utils.js";

export type DaAvailabilityTransactionAction =
  | "open"
  | "publish"
  | "settle"
  | "close"
  | "timeout"
  | "prune"
  | "remove";
export type DaAvailabilityDeployment = {
  readonly contracts: Pick<
    MidgardValidators,
    "availabilityChallenge" | "stateQueue" | "correctionLock"
  >;
  readonly hubOraclePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
  readonly parameters: Availability.DaAvailabilityParameters;
  readonly referenceScripts: Readonly<Record<string, UTxO>>;
  readonly hubOracleRefInput: UTxO;
};
export type DaAvailabilityTransactionResources = {
  readonly collateralInputs: readonly UTxO[];
  readonly feeLovelace: bigint;
  readonly validFrom: bigint;
  readonly validTo: bigint;
};
export type DaAvailabilityExpectedOutput = {
  readonly address: string;
  readonly assets: Assets;
  readonly datum?: string;
};
export type BuiltDaAvailabilityTransaction = {
  readonly tx: TxSignBuilder;
  readonly unsignedCbor: string;
  readonly txId: string;
  readonly action: DaAvailabilityTransactionAction;
  readonly headerHash: string;
  readonly challengeAssetName: string;
  readonly validityRange: {
    readonly validFrom: bigint;
    readonly validTo: bigint;
  };
  readonly spentOutRefs: readonly UTxO[];
  readonly referenceOutRefs: readonly UTxO[];
  readonly collateralOutRefs: readonly UTxO[];
  readonly expectedOutputs: readonly DaAvailabilityExpectedOutput[];
  readonly feeLovelace: bigint;
};
export type DaAvailabilityChallengeSnapshot = {
  readonly headerHash: string;
  readonly bond?: UTxO;
  readonly bondDatum?: Availability.DaAvailabilityBondDatum;
  readonly queue?: StateQueueUTxO;
  readonly confirmedState: StateQueueUTxO;
  readonly descendant?: StateQueueUTxO;
  readonly correctionLock: UTxO;
  readonly terminal?: UTxO;
  readonly terminalDatum?: Availability.DaAvailabilityTerminalAccumulatorDatum;
  readonly tranches: readonly {
    readonly utxo: UTxO;
    readonly datum: Availability.DaAvailabilityTrancheDatum;
    readonly carrier?: UTxO;
  }[];
};
export type OpenDaAvailabilityChallengeParams =
  DaAvailabilityTransactionResources & {
    readonly bond: UTxO;
    readonly queue: UTxO;
    readonly challengerFunding: UTxO;
    readonly challenger: string;
  };
export type PublishDaAvailabilityChunkParams =
  DaAvailabilityTransactionResources & {
    readonly thread: UTxO;
    readonly previousCarrier?: UTxO;
    readonly publication: Availability.DaAvailabilityPublicationDatum;
  };
export type SettleDaAvailabilityTrancheParams =
  DaAvailabilityTransactionResources & {
    readonly bond: UTxO;
    readonly terminal: UTxO;
    readonly thread: UTxO;
    readonly carrier?: UTxO;
  };
export type CloseDaAvailabilityChallengeParams =
  DaAvailabilityTransactionResources & {
    readonly bond: UTxO;
    readonly terminal: UTxO;
    readonly queue: UTxO;
  };
export type DaAvailabilityRemovalParams = DaAvailabilityTransactionResources & {
  readonly queue: UTxO;
  readonly confirmedState: UTxO;
  readonly descendant?: UTxO;
  readonly correctionLock: UTxO;
  readonly challengeAssetName: string;
  readonly headerHash: string;
  readonly rentRefundAddress: string;
  readonly feeFunding?: UTxO;
  readonly fundingQueueTailRefInput?: UTxO;
};
export type TimeoutDaAvailabilityChallengeParams =
  DaAvailabilityRemovalParams & {
    readonly bond: UTxO;
    readonly terminal: UTxO;
  };

export class DaAvailabilityTransactionError extends Error {
  readonly name = "DaAvailabilityTransactionError";
}
const fail = (message: string): never => {
  throw new DaAvailabilityTransactionError(message);
};
const effect = <A>(
  body: () => Promise<A>,
): Effect.Effect<A, DaAvailabilityTransactionError> =>
  Effect.tryPromise({
    try: body,
    catch: (cause) =>
      cause instanceof DaAvailabilityTransactionError
        ? cause
        : new DaAvailabilityTransactionError(
            cause instanceof Error ? cause.message : String(cause),
          ),
  });
const refKey = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;
const inline = (value: string) => ({ kind: "inline" as const, value });
const datum = (u: UTxO): string =>
  u.datum ?? fail(`Missing inline datum on ${refKey(u)}`);
// Providers may return a ledger-normalized CBOR representation. Validate the
// typed Plutus Data value; wire canonicality belongs to signed payload codecs.
const bondDatum = (u: UTxO) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityBondDatum);
  Availability.assertCanonicalDaAvailabilityBondDatum(value);
  return value;
};
const trancheDatum = (u: UTxO) => {
  const value = Data.from(datum(u), Availability.DaAvailabilityTrancheDatum);
  Availability.assertCanonicalDaAvailabilityTrancheDatum(value);
  return value;
};
const terminalDatum = (u: UTxO) => {
  const value = Data.from(
    datum(u),
    Availability.DaAvailabilityTerminalAccumulatorDatum,
  );
  Availability.assertCanonicalDaAvailabilityTerminalAccumulatorDatum(value);
  return value;
};
const state = (u: UTxO, d: DaAvailabilityDeployment) => {
  if (
    u.address !== d.contracts.stateQueue.spendingScriptAddress ||
    u.scriptRef != null
  )
    fail("Unauthentic state queue address");
  return Effect.runPromise(
    utxoToStateQueueUTxO(u, d.contracts.stateQueue.policyId),
  );
};
const keyAddress = (lucid: LucidEvolution, hash: string) =>
  credentialToAddress(lucid.config().network ?? fail("Missing network"), {
    type: "Key",
    hash,
  });
const sameAssets = (a: Assets, b: Assets) =>
  Object.keys(a).length === Object.keys(b).length &&
  Object.entries(a).every(([u, v]) => b[u] === v);
const auth = (u: UTxO, address: string, units: readonly string[]) => {
  if (
    u.address !== address ||
    u.scriptRef != null ||
    units.some((unit) => u.assets[unit] !== 1n) ||
    Object.entries(u.assets).some(
      ([unit, value]) =>
        unit !== "lovelace" && (!units.includes(unit) || value !== 1n),
    )
  )
    fail(`Unauthentic protocol input ${refKey(u)}`);
  datum(u);
};
const outputIndex = (
  ctx: RedeemerContext,
  o: DaAvailabilityExpectedOutput,
  label: string,
) =>
  requireUniqueOutputIndex(
    ctx.outputs,
    (actual) =>
      actual.address === o.address &&
      sameAssets(actual.assets, o.assets) &&
      (o.datum === undefined
        ? actual.datum === undefined
        : outputDatumCborMatches(actual, o.datum)),
    label,
  );
const spend =
  (
    u: UTxO,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilitySpendRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnSpendPurpose(ctx, u, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilitySpendRedeemer);
  };
const coordinate = (u: UTxO, policy: string): BuildTxWithRedeemer =>
  spend(u, (ctx) => ({
    Coordinate: {
      mint_redeemer_index: requireMintRedeemerIndex(
        ctx,
        policy,
        "availability",
      ),
    },
  }));
const mint =
  (
    policy: string,
    build: (ctx: RedeemerContext) => Availability.DaAvailabilityMintRedeemer,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnMintPurpose(ctx, policy, "availability");
    return Data.to(build(ctx), Availability.DaAvailabilityMintRedeemer);
  };
const queueUpdate =
  (
    u: UTxO,
    policy: string,
    output: DaAvailabilityExpectedOutput,
  ): BuildTxWithRedeemer =>
  (ctx) => {
    requireOwnSpendPurpose(ctx, u, "availability queue");
    return Data.to(
      {
        AvailabilityStatusUpdate: {
          state_queue_input_index: requireInputIndex(ctx, u, "queue"),
          state_queue_output_index: outputIndex(ctx, output, "queue"),
          availability_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            policy,
            "availability",
          ),
        },
      },
      StateQueueSpendRedeemer,
    );
  };
const role = (
  d: DaAvailabilityDeployment,
  name: string,
  script: Script,
): UTxO => {
  const u =
    d.referenceScripts[name] ??
    fail(`Missing authenticated reference script ${name}`);
  if (
    u.assets[referenceScriptAuthUnit(d.referenceScriptAuthPolicyId, name)] !==
      1n ||
    u.scriptRef == null ||
    validatorToScriptHash(u.scriptRef) !== validatorToScriptHash(script)
  )
    fail(`Unauthentic reference script ${name}`);
  return u;
};
const baseRefs = (d: DaAvailabilityDeployment) => [
  role(
    d,
    "availability-challenge spending",
    d.contracts.availabilityChallenge.spendingScript,
  ),
];
const mintRefs = (
  d: DaAvailabilityDeployment,
  arm: "open" | "settle" | "close" | "timeout",
) => [
  ...baseRefs(d),
  role(
    d,
    "availability-challenge minting",
    d.contracts.availabilityChallenge.mintingScript,
  ),
  role(
    d,
    `availability-challenge ${arm} withdrawal`,
    d.contracts.availabilityChallenge.yields[arm].withdrawalScript,
  ),
];
const hub = (d: DaAvailabilityDeployment) => {
  if (
    d.hubOracleRefInput.assets[d.hubOraclePolicyId + HUB_ORACLE_ASSET_NAME] !==
    1n
  )
    fail("Unauthentic hub oracle");
  return d.hubOracleRefInput;
};
const withYield = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  tx: TxBuilder,
  arm: "open" | "settle" | "close" | "timeout",
) =>
  tx.withdraw(
    scriptRewardAddress(
      lucid.config().network ?? fail("Missing network"),
      d.contracts.availabilityChallenge.yields[arm].withdrawalScript,
    ),
    0n,
    Data.void(),
  );
const pay = (tx: TxBuilder, outputs: readonly DaAvailabilityExpectedOutput[]) =>
  outputs.reduce(
    (next, o) =>
      o.datum === undefined
        ? next.pay.ToAddress(o.address, o.assets)
        : next.pay.ToContract(o.address, inline(o.datum), o.assets),
    tx,
  );
const minAda = (lucid: LucidEvolution, o: DaAvailabilityExpectedOutput) =>
  calculateMinLovelaceFromUTxO(
    lucid.config().protocolParameters?.coinsPerUtxoByte ??
      fail("Missing live protocol parameters"),
    { ...o, txHash: "00".repeat(32), outputIndex: 0 },
  );

const alignResources = <P extends DaAvailabilityTransactionResources>(
  lucid: LucidEvolution,
  p: P,
): P => {
  const lowerSlot = lucid.unixTimeToSlot(Number(p.validFrom));
  const lower = BigInt(lucid.slotToUnixTime(lowerSlot));
  const validFrom =
    lower < p.validFrom ? BigInt(lucid.slotToUnixTime(lowerSlot + 1)) : lower;
  const validTo = BigInt(
    lucid.slotToUnixTime(lucid.unixTimeToSlot(Number(p.validTo))),
  );
  if (validTo <= validFrom)
    fail("Validity interval contains no complete ledger slot");
  return { ...p, validFrom, validTo };
};

const complete = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityTransactionResources,
  tx: TxBuilder,
  meta: {
    action: DaAvailabilityTransactionAction;
    headerHash: string;
    challengeAssetName: string;
    inputs: readonly UTxO[];
    refs: readonly UTxO[];
    outputs: readonly DaAvailabilityExpectedOutput[];
  },
): Promise<BuiltDaAvailabilityTransaction> => {
  Availability.assertCanonicalDaAvailabilityParameters(d.parameters);
  const cap =
    meta.action === "publish"
      ? d.parameters.max_publication_fee_lovelace
      : meta.action === "settle"
        ? d.parameters.max_settlement_fee_lovelace
        : meta.action === "open"
          ? d.parameters.max_open_fee_lovelace
          : meta.action === "close"
            ? d.parameters.max_close_fee_lovelace
            : d.parameters.max_timeout_fee_lovelace;
  if (
    p.feeLovelace <= 0n ||
    p.feeLovelace > cap ||
    p.validFrom < 0n ||
    p.validTo <= p.validFrom ||
    p.validTo - p.validFrom > 120_000n ||
    p.validTo > BigInt(Number.MAX_SAFE_INTEGER)
  )
    fail("Invalid fee or bounded validity interval");
  const walletAddress = await lucid.wallet().address();
  if (
    p.collateralInputs.length === 0 ||
    p.collateralInputs.some(
      (u) => !isPlainPositiveAdaOnlyUtxo(u) || u.address !== walletAddress,
    )
  )
    fail("Explicit plain-ADA wallet collateral is required");
  const spent = new Set(meta.inputs.map(refKey));
  const refs = new Set(meta.refs.map(refKey));
  if (
    spent.size !== meta.inputs.length ||
    p.collateralInputs.some(
      (u) => spent.has(refKey(u)) || refs.has(refKey(u)),
    ) ||
    meta.inputs.some((u) => refs.has(refKey(u)))
  )
    fail("Transaction resources overlap");
  const available = await lucid.utxosByOutRef([
    ...meta.inputs,
    ...meta.refs,
    ...p.collateralInputs,
  ]);
  const observed = new Map(available.map((u) => [refKey(u), u]));
  for (const u of [...meta.inputs, ...meta.refs, ...p.collateralInputs]) {
    const live = observed.get(refKey(u));
    if (
      !live ||
      live.address !== u.address ||
      !sameAssets(live.assets, u.assets) ||
      (u.datum == null
        ? live.datum != null
        : !outputDatumCborMatches(live, u.datum)) ||
      (live.scriptRef ? validatorToScriptHash(live.scriptRef) : null) !==
        (u.scriptRef ? validatorToScriptHash(u.scriptRef) : null)
    )
      fail(`Stale transaction resource ${refKey(u)}`);
  }
  for (const o of meta.outputs)
    if ((o.assets.lovelace ?? 0n) < minAda(lucid, o))
      fail("Protected output is below live ledger minimum ADA");
  const protocol =
    lucid.config().protocolParameters ??
    fail("Missing live protocol parameters");
  const collateral =
    (p.feeLovelace * BigInt(protocol.collateralPercentage) + 99n) / 100n;
  const completed = await tx
    .setMinFee(p.feeLovelace)
    .validFrom(Number(p.validFrom))
    .validTo(Number(p.validTo))
    .complete({
      ...completeOptionsWithLocalEval({
        coinSelection: false,
        presetWalletInputs: p.collateralInputs,
      }),
      setCollateral: collateral,
    });
  const body = completed.toTransaction().body();
  if (
    body.fee() !== p.feeLovelace ||
    body.inputs().len() !== meta.inputs.length ||
    body.outputs().len() !== meta.outputs.length
  )
    fail("Completed transaction changed protected fee, inputs, or outputs");
  for (let i = 0; i < body.inputs().len(); i++) {
    const u = body.inputs().get(i);
    if (!spent.has(`${u.transaction_id().to_hex()}#${u.index()}`))
      fail("Completed transaction selected an unreserved input");
  }
  const actualCollateral: UTxO[] = [];
  const collateralBody = body.collateral_inputs();
  if (!collateralBody || collateralBody.len() === 0)
    fail("Completed transaction lacks collateral");
  for (let i = 0; i < collateralBody!.len(); i++) {
    const c = collateralBody!.get(i);
    const u = p.collateralInputs.find(
      (u) => refKey(u) === `${c.transaction_id().to_hex()}#${c.index()}`,
    );
    if (!u) fail("Completed transaction selected unreserved collateral");
    actualCollateral.push(u!);
  }
  return {
    tx: completed,
    unsignedCbor: completed.toCBOR(),
    txId: completed.toHash(),
    action: meta.action,
    headerHash: meta.headerHash,
    challengeAssetName: meta.challengeAssetName,
    validityRange: { validFrom: p.validFrom, validTo: p.validTo },
    spentOutRefs: meta.inputs,
    referenceOutRefs: meta.refs,
    collateralOutRefs: actualCollateral,
    expectedOutputs: meta.outputs,
    feeLovelace: p.feeLovelace,
  };
};

/** Conservatively reserves a full-ledger-size carrier and thread at live rent. */
export const assertDaAvailabilityOpeningWorkingCapital = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  plan: Availability.DaAvailabilityChallengeDatumPlan,
): void => {
  const protocol =
    lucid.config().protocolParameters ??
    fail("Missing live protocol parameters");
  // No accepted carrier can be larger than its entire transaction. Encoding this
  // bound as a bytes datum also covers the datum-envelope overhead conservatively.
  const carrierFloor = minAda(lucid, {
    address: d.contracts.availabilityChallenge.spendingScriptAddress,
    assets: { lovelace: 100_000_000n },
    datum: Data.to("00".repeat(protocol.maxTxSize)),
  });
  for (let i = 0; i < plan.trancheThreads.length; i++) {
    const thread = plan.trancheThreads[i]!;
    if (!("Active" in thread)) fail("Opening must create active tranches");
    const active = (
      thread as Extract<
        Availability.DaAvailabilityTrancheDatum,
        { Active: unknown }
      >
    ).Active;
    const funded = plan.trancheFunding[i]!;
    const worstThread = {
      Active: {
        ...active,
        next_offset:
          active.descriptor.start_offset + active.descriptor.byte_length,
        latest_carrier_output_index: 1n,
      },
    };
    const threadFloor = minAda(lucid, {
      address: d.contracts.availabilityChallenge.spendingScriptAddress,
      assets: {
        lovelace: funded.initialLovelace,
        [d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        })]: 1n,
      },
      datum: Data.to(worstThread, Availability.DaAvailabilityTrancheDatum),
    });
    if (
      funded.initialLovelace -
        funded.maximumPublicationFeeReserveLovelace -
        funded.maximumSettlementFeeReserveLovelace <
      carrierFloor + threadFloor
    )
      fail(
        "Challenger bond cannot fund all publication fees and live carrier/thread working capital",
      );
  }
};

export const buildOpenDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: OpenDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const b = bondDatum(p.bond);
    Availability.assertCanonicalDaAvailabilityBondDatum(b, d.parameters);
    if (!("Available" in b))
      fail("Challenge opening requires an available bond");
    const policy = d.contracts.availabilityChallenge.policyId,
      address = d.contracts.availabilityChallenge.spendingScriptAddress;
    const value = (
      b as Extract<Availability.DaAvailabilityBondDatum, { Available: unknown }>
    ).Available;
    auth(p.bond, address, [policy + value.da_bond_asset_name]);
    if (p.bond.assets.lovelace !== d.parameters.da_bond_lovelace)
      fail("Incorrect DA bond value");
    if (
      !isPlainPositiveAdaOnlyUtxo(p.challengerFunding) ||
      p.challengerFunding.address !== keyAddress(lucid, p.challenger) ||
      p.challengerFunding.assets.lovelace !==
        d.parameters.challenger_bond_lovelace + p.feeLovelace
    )
      fail("Opening requires an exact isolated challenger bond plus fee input");
    const q = await state(p.queue, d);
    const node = Data.castFrom(q.datum.data, StateQueueNode);
    if (
      typeof node.da_attestation !== "object" ||
      !("Attested" in node.da_attestation) ||
      node.da_attestation.Attested.da_bond_asset_name !==
        value.da_bond_asset_name ||
      q.assetName !==
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + value.commitment.header_hash
    )
      fail("Queue does not authenticate the available bond");
    const plan = Availability.buildDaAvailabilityChallengeDatumPlan({
      availableBond: b,
      bondInputOutRef: outputReferenceFromUTxO(p.bond),
      challenger: p.challenger,
      openedAt: p.validFrom,
      parameters: d.parameters,
    });
    assertDaAvailabilityOpeningWorkingCapital(lucid, d, plan);
    const outputs: DaAvailabilityExpectedOutput[] = [
      {
        address,
        assets: { ...p.bond.assets, [policy + plan.challengeAssetName]: 1n },
        datum: Availability.encodeDaAvailabilityBondDatum(plan.challengedBond),
      },
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Challenged: {
                da_bond_asset_name: value.da_bond_asset_name,
                challenge_asset_name: plan.challengeAssetName,
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
    ];
    const minted: Assets = { [policy + plan.challengeAssetName]: 1n };
    for (let i = 0; i < plan.trancheThreads.length; i++) {
      const unit =
        policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: plan.challengeAssetName,
          trancheIndex: i,
        });
      minted[unit] = 1n;
      outputs.push({
        address,
        assets: {
          lovelace: plan.trancheFunding[i]!.initialLovelace,
          [unit]: 1n,
        },
        datum: Availability.encodeDaAvailabilityTrancheDatum(
          plan.trancheThreads[i]!,
        ),
      });
    }
    const terminalUnit =
      policy +
      Availability.daAvailabilityTerminalAccumulatorAssetName(
        plan.challengeAssetName,
      );
    minted[terminalUnit] = 1n;
    outputs.push({
      address,
      assets: {
        lovelace: plan.terminalAccumulatorFundingLovelace,
        [terminalUnit]: 1n,
      },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.terminalAccumulator,
      ),
    });
    const refs = [
      ...mintRefs(d, "open"),
      hub(d),
      role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    ];
    const yieldRef = refs[2]!;
    let tx = lucid
      .newTx()
      .collectFrom([p.bond], coordinate(p.bond, policy))
      .collectFrom([p.challengerFunding])
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[1]!))
      .readFrom(refs)
      .mintAssets(
        minted,
        mint(policy, (ctx) => {
          const first = outputIndex(ctx, outputs[2]!, "first tranche");
          for (let i = 0; i < plan.trancheThreads.length; i++)
            if (
              outputIndex(ctx, outputs[2 + i]!, "tranche") !==
              first + BigInt(i)
            )
              fail("Tranche outputs are not contiguous");
          return {
            OpenChallenge: {
              yield_to_ref_input_index: requireReferenceInputIndex(
                ctx,
                yieldRef,
                "open yield",
              ),
              hub_oracle_ref_input_index: requireReferenceInputIndex(
                ctx,
                d.hubOracleRefInput,
                "hub",
              ),
              bond_input_index: requireInputIndex(ctx, p.bond, "bond"),
              bond_output_index: outputIndex(ctx, outputs[0]!, "bond"),
              challenger_input_index: requireInputIndex(
                ctx,
                p.challengerFunding,
                "challenger",
              ),
              state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
              state_queue_output_index: outputIndex(ctx, outputs[1]!, "queue"),
              first_tranche_output_index: first,
              terminal_accumulator_output_index: outputIndex(
                ctx,
                outputs.at(-1)!,
                "terminal",
              ),
              challenger: p.challenger,
            },
          };
        }),
      )
      .addSignerKey(p.challenger);
    tx = withYield(lucid, d, pay(tx, outputs), "open");
    return complete(lucid, d, p, tx, {
      action: "open",
      headerHash: value.commitment.header_hash,
      challengeAssetName: plan.challengeAssetName,
      inputs: [p.bond, p.challengerFunding, p.queue],
      refs,
      outputs,
    });
  });

const authenticateCarrier = (
  d: DaAvailabilityDeployment,
  thread: UTxO,
  t: Availability.DaAvailabilityTrancheDatum,
  carrier?: UTxO,
) => {
  const v = "Active" in t ? t.Active : t.Receipt;
  const index =
    "Active" in t
      ? t.Active.latest_carrier_output_index
      : t.Receipt.terminal_carrier_output_index;
  auth(thread, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId +
      Availability.daAvailabilityTrancheAssetName({
        challengeAssetName: v.challenge_asset_name,
        trancheIndex: Number(v.descriptor.tranche_index),
      }),
  ]);
  if (index === null) {
    if (carrier) fail("Unexpected carrier for a fresh tranche");
    return;
  }
  if (
    !carrier ||
    carrier.txHash !== thread.txHash ||
    BigInt(carrier.outputIndex) !== index
  )
    fail("Missing exact latest carrier out-reference");
  const c = carrier!;
  auth(c, thread.address, []);
  const publication = Availability.parseDaAvailabilityPublicationDatumCbor(
    Data.to(
      Data.from(datum(c), Availability.DaAvailabilityPublicationDatum),
      Availability.DaAvailabilityPublicationDatum,
    ),
    d.parameters.response_geometry,
    v.descriptor,
  );
  const accumulator =
    "Active" in t ? t.Active.accumulator : t.Receipt.terminal_accumulator;
  if (
    publication.challenge_asset_name !== v.challenge_asset_name ||
    publication.header_hash !== v.header_hash ||
    publication.deployment_identity !== v.deployment_identity ||
    publication.tranche_index !== v.descriptor.tranche_index ||
    publication.next_accumulator !== accumulator ||
    publication.chunk_offset + publication.chunk_byte_length !==
      ("Active" in t
        ? t.Active.next_offset
        : t.Receipt.descriptor.start_offset + t.Receipt.descriptor.byte_length)
  )
    fail("Carrier does not authenticate tranche continuation");
};
export const buildPublishDaAvailabilityChunkTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: PublishDaAvailabilityChunkParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.previousCarrier);
    if (!("Active" in t)) fail("Only an active tranche accepts publication");
    const active = (
      t as Extract<Availability.DaAvailabilityTrancheDatum, { Active: unknown }>
    ).Active;
    const carrier: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { lovelace: 0n },
      datum: Availability.encodeDaAvailabilityPublicationDatum(
        p.publication,
        d.parameters.response_geometry,
        active.descriptor,
      ),
    };
    carrier.assets.lovelace = minAda(lucid, carrier);
    const next = Availability.advanceDaAvailabilityTranche({
      active: t,
      publication: p.publication,
      responseGeometry: Availability.availabilityResponseGeometry({
        chunkByteLength: Number(
          d.parameters.response_geometry.chunk_byte_length,
        ),
        trancheByteLength: Number(
          d.parameters.response_geometry.tranche_byte_length,
        ),
        maxTrancheCount: Number(
          d.parameters.response_geometry.max_tranche_count,
        ),
      }),
      inclusiveValidityUpper: p.validTo - 1n,
      carrierOutputIndex: 1n,
    });
    const output: DaAvailabilityExpectedOutput = {
      address: p.thread.address,
      assets: { ...p.thread.assets },
      datum: Availability.encodeDaAvailabilityTrancheDatum(next),
    };
    const transition =
      Availability.planDaAvailabilityPublicationValueTransition({
        threadInputLovelace: p.thread.assets.lovelace,
        previousCarrierInputLovelace: p.previousCarrier?.assets.lovelace ?? 0n,
        nextCarrierOutputLovelace: carrier.assets.lovelace,
        transactionFeeLovelace: p.feeLovelace,
        minimumThreadOutputLovelace: minAda(lucid, output),
        isFirstPublication: active.latest_carrier_output_index === null,
        parameters: d.parameters,
      });
    output.assets.lovelace = transition;
    const refs = baseRefs(d);
    let tx = lucid
      .newTx()
      .readFrom(refs)
      .collectFrom(
        [p.thread],
        spend(p.thread, (ctx) => {
          const carrierIndex = outputIndex(ctx, carrier, "carrier");
          if (carrierIndex !== 1n) fail("Carrier output position changed");
          return {
            AdvanceTranche: {
              thread_output_index: outputIndex(ctx, output, "thread"),
              carrier_output_index: carrierIndex,
              m_previous_carrier_input_index: p.previousCarrier
                ? requireInputIndex(ctx, p.previousCarrier, "previous carrier")
                : null,
            },
          };
        }),
      );
    if (p.previousCarrier)
      tx = tx.collectFrom(
        [p.previousCarrier],
        spend(p.previousCarrier, (ctx) => ({
          ConsumeCarrier: {
            thread_input_index: requireInputIndex(ctx, p.thread, "thread"),
            thread_spend_redeemer_index: requireSpendRedeemerIndex(
              ctx,
              p.thread,
              "thread",
            ),
          },
        })),
      );
    const outputs = [output, carrier];
    return complete(lucid, d, p, pay(tx, outputs), {
      action: "publish",
      headerHash: active.header_hash,
      challengeAssetName: active.challenge_asset_name,
      inputs: [p.thread, ...(p.previousCarrier ? [p.previousCarrier] : [])],
      refs,
      outputs,
    });
  });

const challenged = (
  d: DaAvailabilityDeployment,
  bond: UTxO,
  terminal?: UTxO,
) => {
  const b = bondDatum(bond);
  Availability.assertCanonicalDaAvailabilityBondDatum(b, d.parameters);
  if (!("ChallengedBond" in b)) fail("Expected a challenged bond");
  const v = (
    b as Extract<
      Availability.DaAvailabilityBondDatum,
      { ChallengedBond: unknown }
    >
  ).ChallengedBond;
  if (v.commitment.deployment_identity !== d.hubOraclePolicyId)
    fail("Bond deployment identity mismatch");
  auth(bond, d.contracts.availabilityChallenge.spendingScriptAddress, [
    d.contracts.availabilityChallenge.policyId + v.da_bond_asset_name,
    d.contracts.availabilityChallenge.policyId + v.challenge_asset_name,
  ]);
  if (bond.assets.lovelace !== d.parameters.da_bond_lovelace)
    fail("Incorrect DA bond value");
  if (terminal) {
    const t = terminalDatum(terminal);
    auth(terminal, bond.address, [
      d.contracts.availabilityChallenge.policyId +
        Availability.daAvailabilityTerminalAccumulatorAssetName(
          v.challenge_asset_name,
        ),
    ]);
    if (
      t.challenge_asset_name !== v.challenge_asset_name ||
      t.header_hash !== v.commitment.header_hash ||
      t.deployment_identity !== v.commitment.deployment_identity ||
      t.challenger !== v.challenger ||
      t.response_deadline !== v.response_deadline ||
      t.remaining_challenger_lovelace !== terminal.assets.lovelace
    )
      fail("Terminal accumulator does not authenticate challenged bond");
  }
  return v;
};
export const buildSettleDaAvailabilityTrancheTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: SettleDaAvailabilityTrancheParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const b = challenged(d, p.bond, p.terminal),
      t = trancheDatum(p.thread);
    authenticateCarrier(d, p.thread, t, p.carrier);
    const term = terminalDatum(p.terminal);
    const plan = Availability.planDaAvailabilitySettlement({
      commitment: b.commitment,
      terminalAccumulator: term,
      tranche: t,
      threadLovelace: p.thread.assets.lovelace,
      carrierLovelace: p.carrier?.assets.lovelace ?? 0n,
      transactionFeeLovelace: p.feeLovelace,
      inclusiveValidityLower: p.validFrom,
      parameters: d.parameters,
    });
    const policy = d.contracts.availabilityChallenge.policyId;
    const output = {
      address: p.terminal.address,
      assets: { ...p.terminal.assets, lovelace: plan.nextTerminalLovelace },
      datum: Availability.encodeDaAvailabilityTerminalAccumulatorDatum(
        plan.nextTerminalAccumulator,
      ),
    };
    const refs = [...mintRefs(d, "settle"), p.bond];
    const inputs = [p.terminal, p.thread, ...(p.carrier ? [p.carrier] : [])];
    let tx = lucid.newTx().readFrom(refs);
    for (const u of inputs) tx = tx.collectFrom([u], coordinate(u, policy));
    tx = tx.mintAssets(
      {
        [policy +
        Availability.daAvailabilityTrancheAssetName({
          challengeAssetName: b.challenge_asset_name,
          trancheIndex: Number(term.next_tranche_index),
        })]: -1n,
      },
      mint(policy, (ctx) => ({
        SettleTranche: {
          yield_to_ref_input_index: requireReferenceInputIndex(
            ctx,
            refs[2]!,
            "settle yield",
          ),
          bond_ref_input_index: requireReferenceInputIndex(ctx, p.bond, "bond"),
          terminal_accumulator_input_index: requireInputIndex(
            ctx,
            p.terminal,
            "terminal",
          ),
          terminal_accumulator_output_index: outputIndex(
            ctx,
            output,
            "terminal",
          ),
          tranche_input_index: requireInputIndex(ctx, p.thread, "thread"),
          carrier_input_index: p.carrier
            ? requireInputIndex(ctx, p.carrier, "carrier")
            : null,
        },
      })),
    );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, [output]), "settle"),
      {
        action: "settle",
        headerHash: b.commitment.header_hash,
        challengeAssetName: b.challenge_asset_name,
        inputs,
        refs,
        outputs: [output],
      },
    );
  });
export const buildCloseDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: CloseDaAvailabilityChallengeParams,
) =>
  effect(async () => {
    p = alignResources(lucid, p);
    const b = challenged(d, p.bond, p.terminal),
      term = terminalDatum(p.terminal);
    if (
      term.has_timed_out_tranche ||
      term.next_tranche_index !==
        BigInt(b.commitment.tranche_descriptors.length)
    )
      fail("Challenge is not completely published and settled");
    const q = await state(p.queue, d);
    const node = Data.castFrom(q.datum.data, StateQueueNode);
    if (
      q.assetName !==
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX + b.commitment.header_hash ||
      typeof node.da_attestation !== "object" ||
      !("Challenged" in node.da_attestation) ||
      node.da_attestation.Challenged.challenge_asset_name !==
        b.challenge_asset_name
    )
      fail("Queue does not authenticate the challenge");
    const outputs: DaAvailabilityExpectedOutput[] = [
      {
        address: p.queue.address,
        assets: p.queue.assets,
        datum: encodeLinkedListNodeView({
          ...q.datum,
          data: castStateQueueNodeToData({
            ...node,
            da_attestation: {
              Published: {
                terminal_commitment:
                  Availability.daAvailabilityPublishedTerminalCommitment(
                    b.commitment,
                  ),
              },
            },
          }) as LinkedListNodeView["data"],
        }),
      },
      {
        address: keyAddress(lucid, b.commitment.bond_owner),
        assets: { lovelace: d.parameters.da_bond_lovelace },
      },
      {
        address: keyAddress(lucid, b.challenger),
        assets: {
          lovelace: term.remaining_challenger_lovelace - p.feeLovelace,
        },
      },
    ];
    const policy = d.contracts.availabilityChallenge.policyId,
      refs = [
        ...mintRefs(d, "close"),
        hub(d),
        role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
      ];
    const tx = lucid
      .newTx()
      .collectFrom([p.bond], coordinate(p.bond, policy))
      .collectFrom([p.terminal], coordinate(p.terminal, policy))
      .collectFrom([p.queue], queueUpdate(p.queue, policy, outputs[0]!))
      .readFrom(refs)
      .mintAssets(
        {
          [policy + b.da_bond_asset_name]: -1n,
          [policy + b.challenge_asset_name]: -1n,
          [policy +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            b.challenge_asset_name,
          )]: -1n,
        },
        mint(policy, (ctx) => ({
          CloseChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              refs[2]!,
              "close yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            bond_input_index: requireInputIndex(ctx, p.bond, "bond"),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              p.terminal,
              "terminal",
            ),
            state_queue_input_index: requireInputIndex(ctx, p.queue, "queue"),
            state_queue_output_index: outputIndex(ctx, outputs[0]!, "queue"),
            da_refund_output_index: outputIndex(ctx, outputs[1]!, "DA refund"),
            challenger_refund_output_index: outputIndex(
              ctx,
              outputs[2]!,
              "challenger refund",
            ),
          },
        })),
      );
    return complete(
      lucid,
      d,
      p,
      withYield(lucid, d, pay(tx, outputs), "close"),
      {
        action: "close",
        headerHash: b.commitment.header_hash,
        challengeAssetName: b.challenge_asset_name,
        inputs: [p.bond, p.terminal, p.queue],
        refs,
        outputs,
      },
    );
  });

const removal = async (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
  initial?: { bond: UTxO; terminal: UTxO },
): Promise<BuiltDaAvailabilityTransaction> => {
  p = alignResources(lucid, p);
  const q = await state(p.queue, d),
    root = await state(p.confirmedState, d),
    desc = p.descendant ? await state(p.descendant, d) : undefined;
  if (
    q.assetName !== STATE_QUEUE_NODE_ASSET_NAME_PREFIX + p.headerHash ||
    root.datum.key !== "Empty" ||
    root.datum.next === "Empty" ||
    root.datum.next.Key.key !== p.headerHash
  )
    fail("Unavailable removal requires the current queue head");
  const node = Data.castFrom(q.datum.data, StateQueueNode);
  if (
    typeof node.da_attestation !== "object" ||
    !("Challenged" in node.da_attestation) ||
    node.da_attestation.Challenged.challenge_asset_name !== p.challengeAssetName
  )
    fail("Unavailable queue identity mismatch");
  if (desc) {
    if (
      q.datum.next === "Empty" ||
      desc.datum.key === "Empty" ||
      q.datum.next.Key.key !== desc.datum.key.Key.key
    )
      fail("Removal requires the immediate descendant");
  } else if (q.datum.next !== "Empty")
    fail("Remove head only after its descendants are pruned");
  auth(p.correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  const lock = Data.from(datum(p.correctionLock), CorrectionLockDatum),
    locked: CorrectionLockDatum = {
      Locked: {
        target_header_hash: p.headerHash,
        correction_identity: {
          AvailabilityChallenge: { challenge_asset_name: p.challengeAssetName },
        },
      },
    };
  if (
    initial
      ? lock !== "Idle"
      : Data.to(lock, CorrectionLockDatum) !==
        Data.to(locked, CorrectionLockDatum)
  )
    fail("Correction lock does not authorize this challenge transition");
  const continued = desc ? q : root,
    removed = desc ?? q;
  const outputs: DaAvailabilityExpectedOutput[] = [
    {
      address: continued.utxo.address,
      assets: continued.utxo.assets,
      datum: encodeLinkedListNodeView({
        ...continued.datum,
        next: removed.datum.next,
      }),
    },
    {
      address: p.correctionLock.address,
      assets: p.correctionLock.assets,
      datum: Data.to(desc ? locked : "Idle", CorrectionLockDatum),
    },
  ];
  const inputs = [
    q.utxo,
    ...(desc ? [desc.utxo] : [root.utxo]),
    p.correctionLock,
  ];
  const refs = [
    hub(d),
    role(d, "state-queue spending", d.contracts.stateQueue.spendingScript),
    role(d, "state-queue minting", d.contracts.stateQueue.mintingScript),
    role(
      d,
      "state-queue unavailable-timeout withdrawal",
      d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
    ),
    role(
      d,
      "correction-lock spending",
      d.contracts.correctionLock.spendingScript,
    ),
    ...(desc ? [root.utxo] : []),
  ];
  if (initial && p.fundingQueueTailRefInput) {
    const tail = await state(p.fundingQueueTailRefInput, d);
    if (tail.datum.next !== "Empty")
      fail("Timeout funding witness must be the current queue tail");
    if (![...inputs, ...refs].some((u) => refKey(u) === refKey(tail.utxo)))
      refs.push(tail.utxo);
  }
  const qp = d.contracts.stateQueue.policyId,
    ap = d.contracts.availabilityChallenge.policyId;
  let tx = lucid
    .newTx()
    .collectFrom(
      [q.utxo, ...(desc ? [desc.utxo] : [root.utxo])],
      Data.to("LinkedListMutation", StateQueueSpendRedeemer),
    )
    .collectFrom([p.correctionLock], ((ctx) => {
      requireOwnSpendPurpose(ctx, p.correctionLock, "correction lock");
      return Data.to(
        {
          Correct: {
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
          },
        },
        CorrectionLockRedeemer,
      );
    }) satisfies BuildTxWithRedeemer);
  if (initial) {
    const b = challenged(d, initial.bond, initial.terminal),
      terminal = terminalDatum(initial.terminal);
    if (
      b.challenge_asset_name !== p.challengeAssetName ||
      b.commitment.header_hash !== p.headerHash ||
      !terminal.has_timed_out_tranche ||
      terminal.next_tranche_index !==
        BigInt(b.commitment.tranche_descriptors.length) ||
      p.validFrom < b.response_deadline
    )
      fail(
        "Timeout requires all tranches settled and at least one expired active tranche",
      );
    const challenger = keyAddress(lucid, b.challenger);
    if (p.rentRefundAddress === challenger)
      fail(
        "Queue rent output must be distinct from the two protected challenger payouts",
      );
    outputs.push(
      {
        address: challenger,
        assets: { lovelace: d.parameters.da_bond_lovelace },
      },
      {
        address: challenger,
        assets: {
          lovelace: terminal.remaining_challenger_lovelace - p.feeLovelace,
        },
      },
    );
    refs.push(...mintRefs(d, "timeout"));
    inputs.push(initial.bond, initial.terminal);
    tx = tx
      .collectFrom([initial.bond], coordinate(initial.bond, ap))
      .collectFrom([initial.terminal], coordinate(initial.terminal, ap))
      .mintAssets(
        {
          [ap + b.da_bond_asset_name]: -1n,
          [ap + b.challenge_asset_name]: -1n,
          [ap +
          Availability.daAvailabilityTerminalAccumulatorAssetName(
            b.challenge_asset_name,
          )]: -1n,
        },
        mint(ap, (ctx) => ({
          TimeoutChallenge: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["availability-challenge timeout withdrawal"]!,
              "timeout yield",
            ),
            hub_oracle_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.hubOracleRefInput,
              "hub",
            ),
            bond_input_index: requireInputIndex(ctx, initial.bond, "bond"),
            terminal_accumulator_input_index: requireInputIndex(
              ctx,
              initial.terminal,
              "terminal",
            ),
            state_queue_mint_redeemer_index: requireMintRedeemerIndex(
              ctx,
              qp,
              "queue",
            ),
            da_slash_output_index: outputIndex(ctx, outputs[2]!, "DA slash"),
            challenger_refund_output_index: outputIndex(
              ctx,
              outputs[3]!,
              "challenger refund",
            ),
          },
        })),
      );
    tx = withYield(lucid, d, tx, "timeout");
  } else {
    const funding =
      p.feeFunding ??
      fail("Continuation requires an isolated fee funding input");
    if (
      !isPlainPositiveAdaOnlyUtxo(funding) ||
      funding.assets.lovelace < p.feeLovelace ||
      funding.address !== (await lucid.wallet().address())
    )
      fail("Continuation fee input must cover the explicit fee");
    inputs.push(funding);
    tx = tx.collectFrom([funding]);
    if (funding.assets.lovelace > p.feeLovelace)
      outputs.push({
        address: funding.address,
        assets: { lovelace: funding.assets.lovelace - p.feeLovelace },
      });
  }
  outputs.push({
    address: p.rentRefundAddress,
    assets: { lovelace: removed.utxo.assets.lovelace },
  });
  tx = tx
    .readFrom(refs)
    .mintAssets({ [qp + removed.assetName]: -1n }, ((ctx) => {
      requireOwnMintPurpose(ctx, qp, "unavailable queue removal");
      return Data.to(
        {
          RemoveUnavailableBlockAfterTimeout: {
            yield_to_ref_input_index: requireReferenceInputIndex(
              ctx,
              d.referenceScripts["state-queue unavailable-timeout withdrawal"]!,
              "queue yield",
            ),
            unavailable_header_hash: p.headerHash,
            challenge_asset_name: p.challengeAssetName,
            removal_approach: desc
              ? {
                  PruneTimedOutBlockDescendant: {
                    confirmed_state_ref_input_index: requireReferenceInputIndex(
                      ctx,
                      root.utxo,
                      "root",
                    ),
                    timed_out_node_input_outref: outputReferenceFromUTxO(
                      q.utxo,
                    ),
                    timed_out_node_output_index: outputIndex(
                      ctx,
                      outputs[0]!,
                      "continued unavailable head",
                    ),
                  },
                }
              : {
                  RemoveTimedOutHead: {
                    confirmed_state_input_outref: outputReferenceFromUTxO(
                      root.utxo,
                    ),
                    confirmed_state_output_index: outputIndex(
                      ctx,
                      outputs[0]!,
                      "continued root",
                    ),
                  },
                },
          },
        },
        StateQueueRedeemer,
      );
    }) satisfies BuildTxWithRedeemer)
    .withdraw(
      scriptRewardAddress(
        lucid.config().network ?? fail("Missing network"),
        d.contracts.stateQueue.yields.unavailableTimeout.withdrawalScript,
      ),
      0n,
      Data.void(),
    );
  return complete(lucid, d, p, pay(tx, outputs), {
    action: initial ? "timeout" : desc ? "prune" : "remove",
    headerHash: p.headerHash,
    challengeAssetName: p.challengeAssetName,
    inputs,
    refs,
    outputs,
  });
};
export const buildTimeoutDaAvailabilityChallengeTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: TimeoutDaAvailabilityChallengeParams,
) => effect(() => removal(lucid, d, p, { bond: p.bond, terminal: p.terminal }));
export const buildPruneDaUnavailableBlockDescendantTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (!p.descendant) fail("Pruning requires an immediate descendant");
    return removal(lucid, d, p);
  });
export const buildRemoveDaUnavailableHeadTxProgram = (
  lucid: LucidEvolution,
  d: DaAvailabilityDeployment,
  p: DaAvailabilityRemovalParams,
) =>
  effect(() => {
    if (p.descendant) fail("Head removal cannot include a descendant");
    return removal(lucid, d, p);
  });
export const assertDaAvailabilityReferenceScript = role;

export type DaAvailabilitySnapshotUtxos = {
  readonly availabilityUtxos: readonly UTxO[];
  readonly stateQueueUtxos: readonly UTxO[];
  readonly correctionLockUtxos: readonly UTxO[];
  readonly carrierUtxos?: readonly UTxO[];
};
export const daAvailabilityChallengeSnapshotFromUtxos = async (
  d: DaAvailabilityDeployment,
  headerHash: string,
  utxos: DaAvailabilitySnapshotUtxos,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const policy = d.contracts.availabilityChallenge.policyId,
    address = d.contracts.availabilityChallenge.spendingScriptAddress;
  const byUnit = (
    list: readonly UTxO[],
    unit: string,
    required = false,
  ): UTxO | undefined => {
    const matches = list.filter((u) => (u.assets[unit] ?? 0n) !== 0n);
    if (
      matches.length > 1 ||
      (required && matches.length !== 1) ||
      matches.some((u) => u.assets[unit] !== 1n)
    )
      fail(`Nonunique authenticated unit ${unit}`);
    return matches[0];
  };
  const queueU = byUnit(
    utxos.stateQueueUtxos,
    d.contracts.stateQueue.policyId +
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash,
  );
  const root = await state(
    byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId + STATE_QUEUE_ROOT_ASSET_NAME,
      true,
    )!,
    d,
  );
  const queue = queueU ? await state(queueU, d) : undefined;
  const correctionLock = byUnit(
    utxos.correctionLockUtxos,
    correctionLockUnit(d.hubOraclePolicyId),
    true,
  )!;
  auth(correctionLock, d.contracts.correctionLock.spendingScriptAddress, [
    correctionLockUnit(d.hubOraclePolicyId),
  ]);
  Data.from(datum(correctionLock), CorrectionLockDatum);
  let descendant: StateQueueUTxO | undefined;
  if (queue && queue.datum.next !== "Empty") {
    const u = byUnit(
      utxos.stateQueueUtxos,
      d.contracts.stateQueue.policyId +
        STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        queue.datum.next.Key.key,
      true,
    )!;
    descendant = await state(u, d);
  }
  let bond: UTxO | undefined,
    b: Availability.DaAvailabilityBondDatum | undefined,
    terminal: UTxO | undefined,
    td: Availability.DaAvailabilityTerminalAccumulatorDatum | undefined;
  const tranches: DaAvailabilityChallengeSnapshot["tranches"][number][] = [];
  if (queue) {
    const node = Data.castFrom(queue.datum.data, StateQueueNode),
      status = node.da_attestation;
    if (
      typeof status === "object" &&
      ("Attested" in status || "Challenged" in status)
    ) {
      const v = "Attested" in status ? status.Attested : status.Challenged;
      bond = byUnit(utxos.availabilityUtxos, policy + v.da_bond_asset_name);
      // A locked removal legitimately outlives the burned bond.
      if (!bond) {
        const lock = Data.from(datum(correctionLock), CorrectionLockDatum);
        if (
          typeof lock !== "object" ||
          lock.Locked.target_header_hash !== headerHash ||
          typeof lock.Locked.correction_identity !== "object" ||
          !("AvailabilityChallenge" in lock.Locked.correction_identity) ||
          !("Challenged" in status) ||
          lock.Locked.correction_identity.AvailabilityChallenge
            .challenge_asset_name !== status.Challenged.challenge_asset_name
        )
          fail("Authenticated queue bond is missing");
      } else {
        b = bondDatum(bond);
        Availability.assertCanonicalDaAvailabilityBondDatum(b, d.parameters);
        const bv = "Available" in b ? b.Available : b.ChallengedBond;
        if (
          bv.commitment.header_hash !== headerHash ||
          bv.commitment.deployment_identity !== d.hubOraclePolicyId
        )
          fail("Bond deployment/header mismatch");
        if ("Available" in b) {
          auth(bond, address, [policy + bv.da_bond_asset_name]);
          if (!("Attested" in status)) fail("Queue/bond state mismatch");
        } else {
          if (
            !("Challenged" in status) ||
            status.Challenged.challenge_asset_name !==
              b.ChallengedBond.challenge_asset_name
          )
            fail("Queue/bond challenge mismatch");
          terminal = byUnit(
            utxos.availabilityUtxos,
            policy +
              Availability.daAvailabilityTerminalAccumulatorAssetName(
                b.ChallengedBond.challenge_asset_name,
              ),
            true,
          )!;
          challenged(d, bond, terminal);
          td = terminalDatum(terminal);
          if (
            td.next_tranche_index >
            BigInt(bv.commitment.tranche_descriptors.length)
          )
            fail("Terminal tranche cursor exceeds commitment");
          for (
            let i = Number(td.next_tranche_index);
            i < bv.commitment.tranche_descriptors.length;
            i++
          ) {
            const u = byUnit(
              utxos.availabilityUtxos,
              policy +
                Availability.daAvailabilityTrancheAssetName({
                  challengeAssetName: b.ChallengedBond.challenge_asset_name,
                  trancheIndex: i,
                }),
              true,
            )!;
            const t = trancheDatum(u),
              tv = "Active" in t ? t.Active : t.Receipt;
            if (
              tv.header_hash !== headerHash ||
              tv.deployment_identity !== d.hubOraclePolicyId ||
              tv.descriptor.tranche_index !== BigInt(i) ||
              tv.challenger !== b.ChallengedBond.challenger ||
              ("Active" in t &&
                t.Active.response_deadline !==
                  b.ChallengedBond.response_deadline) ||
              Data.to(
                tv.descriptor,
                Availability.DaAvailabilityTrancheDescriptor,
              ) !==
                Data.to(
                  bv.commitment.tranche_descriptors[i]!,
                  Availability.DaAvailabilityTrancheDescriptor,
                )
            )
              fail("Tranche commitment mismatch");
            const index =
              "Active" in t
                ? t.Active.latest_carrier_output_index
                : t.Receipt.terminal_carrier_output_index;
            const carrier =
              index === null
                ? undefined
                : [
                    ...utxos.availabilityUtxos,
                    ...(utxos.carrierUtxos ?? []),
                  ].find(
                    (c) =>
                      c.txHash === u.txHash && BigInt(c.outputIndex) === index,
                  );
            authenticateCarrier(d, u, t, carrier);
            tranches.push({
              utxo: u,
              datum: t,
              ...(carrier ? { carrier } : {}),
            });
          }
        }
      }
    }
  }
  return {
    headerHash,
    confirmedState: root,
    correctionLock,
    tranches,
    ...(queue ? { queue } : {}),
    ...(descendant ? { descendant } : {}),
    ...(bond ? { bond } : {}),
    ...(b ? { bondDatum: b } : {}),
    ...(terminal ? { terminal } : {}),
    ...(td ? { terminalDatum: td } : {}),
  };
};
export const fetchDaAvailabilityChallengeSnapshot = async (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
): Promise<DaAvailabilityChallengeSnapshot> => {
  const [availabilityUtxos, stateQueueUtxos, correctionLockUtxos] =
    await Promise.all([
      lucid.utxosAt(d.contracts.availabilityChallenge.spendingScriptAddress),
      lucid.utxosAt(d.contracts.stateQueue.spendingScriptAddress),
      lucid.utxosAt(d.contracts.correctionLock.spendingScriptAddress),
    ]);
  return daAvailabilityChallengeSnapshotFromUtxos(d, headerHash, {
    availabilityUtxos,
    stateQueueUtxos,
    correctionLockUtxos,
  });
};
export const fetchDaAvailabilityChallengeSnapshotProgram = (
  lucid: Pick<LucidEvolution, "utxosAt">,
  d: DaAvailabilityDeployment,
  headerHash: string,
) => effect(() => fetchDaAvailabilityChallengeSnapshot(lucid, d, headerHash));
