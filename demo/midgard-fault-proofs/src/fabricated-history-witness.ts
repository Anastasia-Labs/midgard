import {
  admitAuthenticatedL1Observation,
  assertSecurityGradeEvidence,
  type AuthenticatedL1Observation,
  captureEventHistoryWitness,
  countHistoryDataNodes,
  type EventHistoryPayloadBounds,
  type EventHistoryWitness,
  fetchEventHistoryWitness,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  type OutputReference,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { requireSingletonUtxo } from "./runtime.js";
import { governedUserEventAddress } from "./workflow/user-event-address.js";

export type FabricatedHistoryEnvironment = EventHistoryPayloadBounds & {
  readonly retentionAddress: string;
};

/** Raw outputs observed on public L1. Parsed payloads are deliberately not
 * accepted here: every preparation re-derives them from authenticated outputs. */
export type FabricatedHistoryWitness = {
  readonly observation: AuthenticatedL1Observation;
  readonly hubOraclePolicyId: string;
  readonly hubOracleUtxo: UTxO;
  readonly network: Network;
  readonly history: FabricatedHistoryEnvironment;
  readonly anchor: UTxO;
  readonly retainedDataUtxo?: UTxO;
};

const deployment = (
  witness: Omit<FabricatedHistoryWitness, "observation" | "anchor">,
  kind: "Deposit" | "Withdrawal",
) => {
  if (
    witness.history.inlineLimitBytes <= 0n ||
    witness.history.maxPayloadBytes < witness.history.inlineLimitBytes ||
    witness.history.maxPayloadNodes <= 0n
  )
    throw new Error("History proof requires valid explicit payload bounds");
  const hubAddress = credentialToAddress(
    witness.network,
    scriptHashToCredential(witness.hubOraclePolicyId),
  );
  const hub = witness.hubOracleUtxo;
  if (
    hub.address !== hubAddress ||
    hub.assets[toUnit(witness.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME)] !==
      1n ||
    hub.datum == null
  )
    throw new Error(
      "History evidence requires the authentic inline hub oracle",
    );
  const datum = Data.from(hub.datum, HubOracleDatum);
  return {
    stateQueuePolicyId: datum.state_queue,
    deployment: {
      policyId: kind === "Deposit" ? datum.deposit : datum.withdrawal,
      address: governedUserEventAddress(
        witness.network,
        kind === "Deposit" ? datum.deposit_addr : datum.withdrawal_addr,
      ),
      retentionAddress: witness.history.retentionAddress,
      inlineLimitBytes: witness.history.inlineLimitBytes,
    },
  };
};

const capture = (
  witness: EventHistoryWitness,
  policyId: string,
  kind: "Deposit" | "Withdrawal",
  history: FabricatedHistoryEnvironment,
) => {
  if (witness.kind === "Absent") return undefined;
  if (BigInt(witness.payloadCbor.length / 2) > history.maxPayloadBytes)
    throw new Error("History proof payload exceeds its applied byte bound");
  countHistoryDataNodes(
    Data.from(witness.payloadCbor),
    history.maxPayloadNodes,
  );
  return captureEventHistoryWitness(witness, policyId, kind);
};

export const authenticateFabricatedHistoryWitness = async (
  witness: FabricatedHistoryWitness,
  kind: "Deposit" | "Withdrawal",
  id: OutputReference,
  minimumConfirmationDepth?: number,
) => {
  const admitted = admitAuthenticatedL1Observation({
    observation: witness.observation,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  assertSecurityGradeEvidence(admitted.provenance);
  const resolved = deployment(witness, kind);
  // Reuse the public witness verifier, with exactly the supplied observed
  // outputs. This authenticates full keys, strict gaps/equal fillers, inline
  // content and complete external datum hashes rather than caller assertions.
  const current = await fetchEventHistoryWitness(
    {
      utxosAt: async (address) =>
        address === resolved.deployment.address
          ? [witness.anchor]
          : address === resolved.deployment.retentionAddress &&
              witness.retainedDataUtxo
            ? [witness.retainedDataUtxo]
            : [],
    },
    resolved.deployment,
    id,
  );
  return {
    ...resolved,
    witness: current,
    captured: capture(
      current,
      resolved.deployment.policyId,
      kind,
      witness.history,
    ),
  };
};

export const fetchCurrentHistory = async ({
  lucid,
  network,
  hubOraclePolicyId,
  history,
  kind,
  id,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly kind: "Deposit" | "Withdrawal";
  readonly id: OutputReference;
}) => {
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(hubOraclePolicyId),
    ),
    unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "event history hub oracle",
  });
  const common = { network, hubOraclePolicyId, hubOracleUtxo, history };
  const resolved = deployment(common, kind);
  const current = await fetchEventHistoryWitness(
    { utxosAt: (address) => lucid.utxosAt(address) },
    resolved.deployment,
    id,
  );
  return {
    ...common,
    ...resolved,
    witness: current,
    captured: capture(current, resolved.deployment.policyId, kind, history),
  };
};

export const fetchFabricatedHistoryWitness = async (args: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly history: FabricatedHistoryEnvironment;
  readonly observation: AuthenticatedL1Observation;
  readonly kind: "Deposit" | "Withdrawal";
  readonly id: OutputReference;
}): Promise<FabricatedHistoryWitness> => {
  const current = await fetchCurrentHistory(args);
  return {
    network: args.network,
    hubOraclePolicyId: args.hubOraclePolicyId,
    hubOracleUtxo: current.hubOracleUtxo,
    history: args.history,
    observation: args.observation,
    anchor: current.witness.anchor.utxo,
    ...(current.witness.kind === "Present" && current.witness.retainedDataUtxo
      ? { retainedDataUtxo: current.witness.retainedDataUtxo }
      : {}),
  };
};
