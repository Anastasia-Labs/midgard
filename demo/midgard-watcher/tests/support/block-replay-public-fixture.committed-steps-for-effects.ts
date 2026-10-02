import { encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { h32 } from "@al-ft/midgard-test-support/hex";
import {
  buildCanonicalTransitionEffect,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type CanonicalTransitionEffect,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import {
  makeNativeTx,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import type { WatcherStateQueueHeader } from "../../src/indexers/state-queue-snapshot.js";
import {
  type WatcherBlockReplayEventAuthority,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayPriorUtxo,
} from "../../src/verification/block-replay.js";
import { type WatcherHeaderRootReconstructionResult } from "../../src/verification/header-root-reconstruction.js";
import { evaluateWatcherPhaseABlock } from "../../src/verification/phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
  type WatcherRuleBundle,
} from "../../src/verification/rule-bundle.js";

export const entries = (
  values: readonly (readonly [Buffer, Buffer])[],
): readonly WatcherBlockReplayPriorUtxo[] =>
  values.map(([outRef, output]) => ({
    outRef: outRef.toString("hex"),
    outputCbor: output.toString("hex"),
  }));

export const L1_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};

export const DA_PROVENANCE: SDK.EvidenceProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};

export const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

export const RULE_BUNDLE: WatcherRuleBundle = makeWatcherCanonicalRuleBundle({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    blueprintHash: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});

export const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleCommitment(RULE_BUNDLE);

export const headerHashOf = (value: SDK.Header): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(value, SDK.Header), "hex"), { dkLen: 28 }),
  ).toString("hex");

export const sortEntries = (
  values: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...values].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

export const bufferEntries = (values: readonly SDK.DaPayloadEntry[]) =>
  values.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));

export const dataHex = <A>(
  value: A,
  schema: Parameters<typeof Data.to>[1],
): string => encodeData(value, schema as never).toString("hex");

export const cardanoOutputAssets = (
  outputCborHex: string,
): Readonly<Record<string, bigint>> => {
  const value = CML.TransactionOutput.from_cbor_hex(outputCborHex).amount();
  const assets: Record<string, bigint> = { lovelace: value.coin() };
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policies = multiasset.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const policyAssets = multiasset.get_assets(policy);
      if (policyAssets === undefined) continue;
      const names = policyAssets.keys();
      for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
        const name = names.get(nameIndex);
        const quantity = policyAssets.get(name);
        if (quantity !== undefined) {
          assets[`${policy.to_hex()}${name.to_hex()}`] = quantity;
        }
      }
    }
  }
  return Object.freeze(assets);
};

export const nativeEffect = (input: {
  readonly spent: readonly Buffer[];
  readonly native: Pick<ReturnType<typeof makeNativeTx>, "txId" | "txCbor">;
  readonly outputs: readonly Buffer[];
}): CanonicalTransitionEffect =>
  buildCanonicalTransitionEffect([
    ...input.spent.map((outRefCbor) => ({
      type: "delete" as const,
      outRefCbor,
    })),
    ...input.outputs.map((outputCbor, outputIndex) => ({
      type: "insert" as const,
      outRefCbor: outRefFromTxId(input.native.txId, BigInt(outputIndex)),
      outputCbor,
    })),
  ]);

export type CommittedEffectGroup = Readonly<{
  eventKey: SDK.EventKey;
  phase: SDK.TransitionPhase;
  effect: CanonicalTransitionEffect;
}>;

export const committedStepsForEffects = async (
  priorState: readonly WatcherBlockReplayPriorUtxo[],
  groups: readonly CommittedEffectGroup[],
): Promise<readonly SDK.TransitionStep[]> => {
  const prior = await watcherBlockReplayPriorState(priorState);
  const operations: ValidationMachineLedgerOp[] = groups.flatMap(({ effect }) =>
    effect.operations.map((operation) =>
      operation.type === "delete"
        ? { type: "delete" as const, key: operation.outRefCbor }
        : buildValidationMachineLedgerInsertOp({
            key: operation.outRefCbor,
            outputCbor: operation.outputCbor,
          }),
    ),
  );
  const mutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: priorState.map((entry) => ({
      outRef: Buffer.from(entry.outRef, "hex"),
      output: Buffer.from(entry.outputCbor, "hex"),
    })),
    operations,
  });
  let root = prior.root;
  let cursor = 0;
  return Object.freeze(
    groups.map((group, stepIndex) => {
      const preRoot = root;
      cursor += group.effect.operations.length;
      if (group.effect.operations.length > 0) {
        root = mutationSteps[cursor - 1]!.postRoot.toString("hex");
      }
      return Object.freeze({
        schema_version: 1n,
        step_index: BigInt(stepIndex),
        event_key: group.eventKey,
        phase: group.phase,
        pre_utxos_root: preRoot,
        post_utxos_root: root,
      });
    }),
  );
};

export const watcherHeaderRecord = (
  value: SDK.Header,
  headerHash: string,
): WatcherStateQueueHeader => ({
  headerHash,
  headerCborHex: Data.to(value, SDK.Header),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: value.prevUtxosRoot,
  utxosRoot: value.utxosRoot,
  withdrawalsRoot: value.withdrawalsRoot,
  forcedTransactionsRoot: value.forcedTransactionsRoot,
  transactionsRoot: value.transactionsRoot,
  depositsRoot: value.depositsRoot,
  transitionTraceRoot: value.transitionTraceRoot,
  eventToStepRoot: value.eventToStepRoot,
  validationTracesRoot: value.validationTracesRoot,
  withdrawalCount: value.withdrawalCount.toString(),
  forcedTransactionCount: value.forcedTransactionCount.toString(),
  l2TransactionCount: value.l2TransactionCount.toString(),
  depositCount: value.depositCount.toString(),
  totalEventCount: value.totalEventCount.toString(),
  transitionStepCount: value.transitionStepCount.toString(),
  validationTraceCount: value.validationTraceCount.toString(),
  startTime: value.startTime.toString(),
  endTime: value.endTime.toString(),
  blockSlot: value.blockSlot.toString(),
  expectedNetworkId: value.expectedNetworkId.toString(),
  minFeeA: value.minFeeA.toString(),
  minFeeB: value.minFeeB.toString(),
  prevHeaderHash: value.prevHeaderHash,
  operatorVkey: value.operatorVkey,
  protocolVersion: value.protocolVersion.toString(),
  daAttestationPolicyId: null,
});

export type PublicFixtureEvent = Readonly<{
  eventKey: SDK.EventKey;
  phase: Exclude<SDK.TransitionPhase, "L2Transaction">;
  domain: "withdrawals" | "forced_transactions" | "deposits";
  entry: SDK.DaPayloadEntry;
  forcedPreimage?: SDK.DaPayloadEntry;
}>;

export type PublicReplayFixture = Readonly<{
  observation: SDK.AuthenticatedStateQueueHeaderObservation;
  reconstruction: WatcherHeaderRootReconstructionResult;
  phaseA: Awaited<ReturnType<typeof evaluateWatcherPhaseABlock>>;
  envelope: Buffer;
  priorState: readonly WatcherBlockReplayPriorUtxo[];
  eventAuthorities: readonly WatcherBlockReplayEventAuthority[];
  header: SDK.Header;
  ruleBundle: WatcherRuleBundle;
}>;

export const publicInput = (fixture: PublicReplayFixture) => ({
  observation: fixture.observation,
  reconstruction: fixture.reconstruction,
  phaseA: fixture.phaseA,
  payloadEnvelopeCbor: fixture.envelope,
  daProvenance: DA_PROVENANCE,
  priorState: fixture.priorState,
  eventAuthorities: fixture.eventAuthorities,
  ruleBundle: fixture.ruleBundle,
  ruleBundleCommitment: computeWatcherRuleBundleCommitment(fixture.ruleBundle),
});
