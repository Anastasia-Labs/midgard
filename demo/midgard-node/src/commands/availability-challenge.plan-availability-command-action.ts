import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type DeploymentManifest } from "./contract-deployment-info.js";

export type AvailabilityCommandAction =
  | "open"
  | "respond"
  | "settle"
  | "close"
  | "timeout"
  | "status"
  | "recover";

export type AvailabilityCommandOptions = Readonly<{
  manifest: string;
  journal: string;
  headerHash: string;
  walletSeedEnv: string;
  collateralOutRef?: string;
  fundingOutRef?: string;
  payloadFile?: string;
  trancheIndex?: number;
}>;

export const parseAvailabilityOutRef = (
  value: string,
): { txHash: string; outputIndex: number } => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(value);
  const outputIndex = Number(match?.[2]);
  if (!match || !Number.isSafeInteger(outputIndex) || outputIndex > 65_535)
    throw new Error(
      "Availability output references must be canonical txHash#outputIndex",
    );
  return { txHash: match[1]!, outputIndex };
};

export const planAvailabilityCommandAction = (
  action: Exclude<AvailabilityCommandAction, "status" | "recover">,
  snapshot: Pick<
    SDK.DaAvailabilityChallengeSnapshot,
    | "recordDatum"
    | "terminalDatum"
    | "correctionLock"
    | "headerHash"
    | "descendant"
  >,
  nowMs: number,
): SDK.DaAvailabilityTransactionAction => {
  if (action === "respond") return "publish";
  if (action !== "timeout") return action;
  if (snapshot.recordDatum) {
    const record = snapshot.recordDatum;
    if (BigInt(nowMs) <= record.response_deadline)
      throw new Error(
        "Availability timeout requires the strict response deadline to have passed",
      );
    if (!snapshot.terminalDatum)
      throw new Error(
        "Availability timeout is missing its terminal accumulator",
      );
    if (
      snapshot.terminalDatum.next_tranche_index <
      BigInt(record.commitment.tranche_descriptors.length)
    )
      return "settle";
    if (!snapshot.terminalDatum.has_timed_out_tranche)
      throw new Error(
        "Fully answered availability challenges must close instead of timing out",
      );
    return "timeout";
  }
  const lock = snapshot.correctionLock.datum
    ? Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum)
    : undefined;
  if (
    typeof lock !== "object" ||
    !lock ||
    !("Locked" in lock) ||
    lock.Locked.target_header_hash !== snapshot.headerHash ||
    typeof lock.Locked.correction_identity !== "object" ||
    !("AvailabilityChallenge" in lock.Locked.correction_identity)
  ) {
    throw new Error("Availability timeout has no matching active removal lock");
  }
  return snapshot.descendant ? "prune" : "remove";
};

export const required = <T>(value: T | undefined, name: string): T => {
  if (value === undefined)
    throw new Error(`Availability action requires ${name}`);
  return value;
};

export const liveOutRef = async (
  lucid: LucidEvolution,
  label: string | undefined,
  name: string,
): Promise<UTxO> => {
  const reference = parseAvailabilityOutRef(required(label, name));
  const utxos = await lucid.utxosByOutRef([reference]);
  if (utxos.length !== 1)
    throw new Error(`Availability ${name} is not currently unspent`);
  return utxos[0]!;
};

export const assertAvailabilityCommandRemovalCapital = (input: {
  readonly action: "open" | "timeout" | "prune" | "remove";
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly remainingRemovalSteps: number;
  readonly minimumChangeLovelace: bigint;
  readonly walletAddress: string;
  readonly walletUtxos: readonly UTxO[];
  readonly collateral: UTxO;
  readonly reservedOutRefs: ReadonlySet<string>;
}): void => {
  if (
    !Number.isSafeInteger(input.remainingRemovalSteps) ||
    input.remainingRemovalSteps < 1
  )
    throw new Error(
      "Availability capital requires an authenticated remaining removal path",
    );
  const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
  const available = input.walletUtxos.filter(
    (utxo) =>
      utxo.address === input.walletAddress &&
      utxo.datum == null &&
      utxo.datumHash == null &&
      utxo.scriptRef == null &&
      Object.keys(utxo.assets).length === 1 &&
      (utxo.assets.lovelace ?? 0n) > 0n &&
      outRef(utxo) !== outRef(input.collateral) &&
      !input.reservedOutRefs.has(outRef(utxo)),
  );
  const requiredCapital =
    BigInt(input.remainingRemovalSteps) *
      input.parameters.max_timeout_fee_lovelace +
    input.minimumChangeLovelace +
    (input.action === "open"
      ? input.parameters.challenger_bond_lovelace +
        input.parameters.challenge_record_lovelace +
        input.parameters.max_open_fee_lovelace
      : 0n);
  if (
    available.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n) <
    requiredCapital
  )
    throw new Error(
      "Availability actor cannot fund the challenger bond, the challenge record and the remaining descendant removal path after excluding collateral and reserved inputs",
    );
};

/**
 * The collateral a Timeout must post in the worst case: the ledger's
 * collateral percentage of the largest Timeout fee. That fee is the slashed
 * part, at most `da_slash_penalty_lovelace`, plus the challenger's part, at
 * most `max_timeout_fee_lovelace`. On Cardano's 150% this is
 * `1.5 × (penalty + max_timeout_fee)`.
 */
export const availabilityTimeoutCollateralLovelace = (input: {
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly collateralPercentage: number;
}): bigint => {
  if (
    !Number.isSafeInteger(input.collateralPercentage) ||
    input.collateralPercentage <= 0
  )
    throw new Error(
      "Availability timeout collateral needs a positive ledger collateral percentage",
    );
  const fee =
    input.parameters.da_slash_penalty_lovelace +
    input.parameters.max_timeout_fee_lovelace;
  return (fee * BigInt(input.collateralPercentage) + 99n) / 100n;
};

/** Refuses a Timeout collateral coin that cannot cover the worst-case fee. */
export const assertAvailabilityTimeoutCollateral = (input: {
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly collateralPercentage: number;
  readonly collateral: UTxO;
}): void => {
  const requiredLovelace = availabilityTimeoutCollateralLovelace(input);
  const held = input.collateral.assets.lovelace ?? 0n;
  if (held < requiredLovelace)
    throw new Error(
      `Availability timeout collateral holds ${held.toString()} lovelace; it needs at least ${requiredLovelace.toString()} (${input.collateralPercentage.toString()}% of the largest timeout fee, da_slash_penalty_lovelace + max_timeout_fee_lovelace)`,
    );
};

/**
 * Where a Timeout returns the queue node's rent. The builder refuses the
 * challenger's own enterprise key address, which already receives the one
 * protected challenger output, so the rent goes to the actor's base address
 * (payment and stake both the actor key). The actor controls both.
 */
export const availabilityTimeoutRentRefundAddress = (
  network: Network,
  actor: string,
): string =>
  credentialToAddress(
    network,
    { type: "Key", hash: actor },
    { type: "Key", hash: actor },
  );

/**
 * The inline datums of every retained output that ever held one unit, live or
 * spent (`availabilityStoreUnitHistory` over the node's follower store).
 */
export type AvailabilityUnitHistory = (
  input: Readonly<{ policyId: string; assetName: string }>,
) => Promise<readonly (string | null)[]>;

/**
 * Recovers the commitment preimage an Open needs. After Apply the queue node
 * keeps only `commitment_hash`; the full commitment lives in the DA
 * attestation datum that Apply spent. This reads every retained output that
 * ever held the block's DAAT token and returns the commitment whose hash
 * equals the node's. The hash authenticates the answer, and the SDK Open
 * builder checks it again against the node.
 */
export const recoverAvailabilityOpenCommitment = async (input: {
  readonly unitHistory: AvailabilityUnitHistory;
  readonly daAttestationPolicyId: string;
  readonly headerHash: string;
  readonly commitmentHash: string;
}): Promise<SDK.DaAvailabilityCommitment> => {
  const datums = await input.unitHistory({
    policyId: input.daAttestationPolicyId,
    assetName: SDK.daAttestationAssetName(input.headerHash),
  });
  for (const datum of datums) {
    if (datum === null) continue;
    try {
      const attestation = Data.from(datum, SDK.DaAttestationDatum);
      if (
        attestation.header_hash === input.headerHash &&
        SDK.daAvailabilityCommitmentHash(
          attestation.availability_commitment,
        ) === input.commitmentHash
      )
        return attestation.availability_commitment;
    } catch {
      // Not a DA attestation datum: it cannot be the commitment's source.
    }
  }
  throw new Error(
    `No retained DA attestation output for header ${input.headerHash} holds a commitment hashing to ${input.commitmentHash}; the follower store prunes spent outputs past its retention window`,
  );
};

/** The deployment facts the command builder reads beyond the SDK deployment. */
export type AvailabilityCommandBuildContext = Readonly<{
  daChallengeWindowMs: bigint;
  /** Absent when the manifest records no DA attestation policy. */
  daAttestationPolicyId: string | undefined;
  /** The unit history Open reads the commitment preimage from. */
  unitHistory: AvailabilityUnitHistory;
}>;

export const availabilityCommandBuildContext = (
  manifest: DeploymentManifest,
  unitHistory: AvailabilityUnitHistory,
): AvailabilityCommandBuildContext => ({
  daChallengeWindowMs: BigInt(
    manifest.deploymentProfile.timing.da_challenge_window_ms,
  ),
  daAttestationPolicyId: manifest.contracts.daAttestationMint?.scriptHash,
  unitHistory,
});
