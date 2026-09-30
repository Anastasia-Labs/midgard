import { hashMidgardVersionedScript } from "@al-ft/midgard-core";
import {
  decodeMidgardVersionedScript,
  encodeCbor,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type FraudProofCatalogueCategoryName,
  type RejectionReason,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import { type CompleteCanonicalReplay } from "../src/workflow/complete-replay.js";
import type { TypedReasonArm } from "../src/workflow/reason-disposition.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";
import {
  type FixtureTransactionInput,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

export const deploymentFingerprint = "d1".repeat(32);

export const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };

export const releaseFinalityAuthority = {
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: deploymentFingerprint,
    blueprintHash: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  }),
};

export const base: FixtureTransactionInput = {
  spendInputs: [outRefCbor(71, 0n)],
  fee: 7n,
  networkId: 0n,
};

export type ReasonCase = {
  readonly arm: TypedReasonArm;
  readonly category: FraudProofCatalogueCategoryName;
  readonly replayer: CompleteCanonicalReplay;
  readonly reason: RejectionReason;
  readonly accepted: Partial<FixtureTransactionInput>;
  readonly wrongful: Partial<FixtureTransactionInput>;
  readonly mismatchFieldIndex?: number;
  readonly minFeeB?: bigint;
  readonly acceptedWitness?: "valid" | "invalid";
  readonly wrongfulWitness?: "valid" | "invalid";
  readonly priorLedger?: true;
  readonly priorOutput?: Buffer;
  readonly acceptedApplicability?:
    | "unreachable_under_consensus_bounds"
    | "not_exercised";
};

export const canonicalRedeemer = encodeMidgardRedeemerWitnessItem({
  purpose: "Spend",
  index: 0n,
  redeemerCbor: Buffer.from("00", "hex"),
  executionUnits: { memory: 1n, steps: 2n },
});

export const nativeTrueScript = Buffer.from("820043820180", "hex");

export const nativeFalseScript = Buffer.from("820043820280", "hex");

export const nativeMint = (
  scriptWitness: Buffer,
): Partial<FixtureTransactionInput> => ({
  scriptWitnesses: [scriptWitness],
  mintPolicyItems: [
    encodeCbor([
      Buffer.from(
        hashMidgardVersionedScript(decodeMidgardVersionedScript(scriptWitness)),
        "hex",
      ),
      new Map([[Buffer.alloc(0), 1n]]),
    ]),
  ],
});

export const nativeSignatureScript = Buffer.concat([
  Buffer.from("820058208200581c", "hex"),
  Buffer.alloc(28, 0x99),
]);

const witnessSeed = Buffer.alloc(32);

witnessSeed.writeUInt32BE(1, 28);

export const signerHash = Buffer.from(
  CML.PrivateKey.from_normal_bytes(witnessSeed)
    .to_public()
    .hash()
    .to_raw_bytes(),
);

export const output = (lovelace = 2_000_000n, protectedOwner = false) =>
  encodeMidgardTxOutput({
    address: Buffer.concat([
      Buffer.from([protectedOwner ? 0x68 : 0x60]),
      signerHash,
    ]),
    value: { lovelace, assets: new Map() },
  });
