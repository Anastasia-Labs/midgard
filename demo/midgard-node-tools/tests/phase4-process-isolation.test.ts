import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { loadPhasMembershipWithdrawalScript } from "midgard-node/phas-membership";
import { describe, expect, it } from "vitest";

import {
  buildPhase4IsolatedChildEnv,
  decodePhase4MatchedSnapshotIdentityV1,
  decodePhase4ResetAttestationV1,
  type Phase4ProcessIsolationIdentity,
  validatePhase4PhasRegistrationProof,
  validatePhase4PhasRegistrationTransactionBody,
  validatePhase4ProcessIsolationValues,
  validatePhase4ResetAttestation,
} from "../src/commands/e2e-pipelined-commit-process-acceptance.js";
import { PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE } from "../src/commands/phase4-genesis-ledger.js";

const values = (): Record<string, string> => ({
  POSTGRES_HOST: "127.0.0.1",
  POSTGRES_PORT: "5544",
  POSTGRES_DB: "midgard_phase4_process_test",
  L1_PROVIDER: "Kupmios",
  L1_OGMIOS_KEY: "http://127.0.0.1:2337",
  L1_KUPO_KEY: "http://127.0.0.1:2442",
  MIN_FEE_A: "0",
  MIN_FEE_B: "0",
  RUN_GENESIS_ON_STARTUP: "false",
  MIDGARD_DOTENV_MODE: "disabled",
  NETWORK: "Custom",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_A: "test-only-a",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_B: "test-only-b",
  TESTNET_GENESIS_WALLET_SEED_PHRASE_C: "test-only-a",
  MIDGARD_PHASE4_COMPOSE_PROJECT: "midgard_phase4_process_test",
  MIDGARD_PHASE4_NETWORK_MAGIC: "420042",
});

const canonicalPhasIdentity = SDK.phasMembershipIdentity(
  "Custom",
  loadPhasMembershipWithdrawalScript(),
);
const PHAS_SCRIPT_HASH = canonicalPhasIdentity.scriptHash;
const PHAS_REWARD_ADDRESS = canonicalPhasIdentity.rewardAddress;
const CBOR_TEMPLATE_SCRIPT_HASH =
  "46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4d";
const PHAS_REGISTRATION_TRANSACTION_BODY = {
  type: "Unwitnessed Tx ConwayEra",
  description: "PHAS registration transaction body",
  cborHex:
    "84a400d901028182582000000000000000000000000000000000000000000000000000000000000000000001818258390056256482f4e32203bbf0e61f5c0208f776216707b8c1a198e945149ee41bf07d00d3b340e0ba35ee9c82110e6190de18b6d730577223e6c51b00000006fc0299cb021a00028db504d901028182008201581c46df0027fc0af07197924dc07f1c27ac6b15eb2bd6efc7a73b0dbb4da0f5f6".replace(
      CBOR_TEMPLATE_SCRIPT_HASH,
      PHAS_SCRIPT_HASH,
    ),
} as const;
const PHAS_REGISTRATION_TRANSACTION = CML.Transaction.from_cbor_hex(
  PHAS_REGISTRATION_TRANSACTION_BODY.cborHex,
);
const PHAS_REGISTRATION_TX_HASH = CML.hash_transaction(
  PHAS_REGISTRATION_TRANSACTION.body(),
).to_hex();
const PHAS_REGISTRATION_CBOR_SHA256 = createHash("sha256")
  .update(Buffer.from(PHAS_REGISTRATION_TRANSACTION_BODY.cborHex, "hex"))
  .digest("hex");

const isolation: Phase4ProcessIsolationIdentity = {
  envFile: "/tmp/phase4/phase4.env",
  deploymentManifestPath: "/tmp/phase4/contract-deployment-info.json",
  deploymentManifestSha256: "a".repeat(64),
  snapshotIdentityPath: "/tmp/phase4/snapshot-identity.json",
  snapshotIdentitySha256: "d".repeat(64),
  snapshotCardanoTip: { slot: 1234, hash: "c".repeat(64) },
  snapshotKupoCheckpoint: 1234,
  snapshotBlueprintSha256: "f".repeat(64),
  snapshotPhasRegistrationProofSha256: "9".repeat(64),
  snapshotPhasRegistration: {
    schemaVersion: "midgard-phase4-phas-registration-proof-v1",
    source: "cardano-cli-local-state-query",
    readOnly: true,
    registered: true,
    cardanoImage: {
      ref: `cardano-node@sha256:${"8".repeat(64)}`,
      id: `sha256:${"7".repeat(64)}`,
    },
    networkMagic: 420042,
    manifestId: "6".repeat(64),
    registrationTxHash: PHAS_REGISTRATION_TX_HASH,
    rewardAddress: PHAS_REWARD_ADDRESS,
    rewardAddressBase16: `f0${PHAS_SCRIPT_HASH}`,
    scriptHash: PHAS_SCRIPT_HASH,
    transactionBody: {
      schemaVersion: "midgard-phas-registration-transaction-body-v1",
      artifactSha256:
        "5d19fdf1cebce4c95165dbd317ff582e8c01be67a14e4eed2f13ceb1c9ee9610",
      cborSha256: PHAS_REGISTRATION_CBOR_SHA256,
      cborSizeBytes: 162,
      cardanoCliTxHash: PHAS_REGISTRATION_TX_HASH,
      certificate: {
        kind: "stake_registration",
        index: 0,
        count: 1,
        credentialType: "script",
        scriptHash: PHAS_SCRIPT_HASH,
      },
    },
    registrationDepositLovelace: 400_000,
    confirmation: { slot: 1200, blockHeaderHash: "3".repeat(64) },
    observedAtTip: { slot: 1234, hash: "c".repeat(64) },
  },
  snapshotPhasRegistrationTransactionBody: PHAS_REGISTRATION_TRANSACTION_BODY,
  composeProject: "midgard_phase4_process_test",
  networkMagic: 420042,
  postgresDatabase: "midgard_phase4_process_test",
  postgresPort: 5544,
  ogmiosPort: 2337,
  kupoPort: 2442,
};

const attestation = () => ({
  schemaVersion: "midgard-phase4-local-devnet-reset-attestation-v1",
  scenarioLabel: "crash-speculative_mid_build-flag-on",
  composeProject: isolation.composeProject,
  networkMagic: isolation.networkMagic,
  postgresDatabase: isolation.postgresDatabase,
  deploymentManifestSha256: isolation.deploymentManifestSha256,
  snapshotSetSha256: "b".repeat(64),
  snapshotIdentitySha256: isolation.snapshotIdentitySha256,
  phasRegistrationProofSha256: isolation.snapshotPhasRegistrationProofSha256,
  phasRegistration: isolation.snapshotPhasRegistration,
  cardanoTip: { slot: 1234, hash: "c".repeat(64) },
  kupoCheckpoint: 1234,
});

const snapshotIdentity = () => ({
  schemaVersion: "midgard-phase4-matched-snapshot-identity-v1",
  composeProject: isolation.composeProject,
  networkMagic: isolation.networkMagic,
  postgresDatabase: isolation.postgresDatabase,
  deploymentManifestSha256: isolation.deploymentManifestSha256,
  blueprintSha256: isolation.snapshotBlueprintSha256,
  images: {
    cardanoNode: isolation.snapshotPhasRegistration.cardanoImage,
    ogmios: {
      ref: `ogmios@sha256:${"1".repeat(64)}`,
      id: `sha256:${"2".repeat(64)}`,
    },
    kupo: {
      ref: `kupo@sha256:${"3".repeat(64)}`,
      id: `sha256:${"4".repeat(64)}`,
    },
    postgres: {
      ref: `postgres@sha256:${"5".repeat(64)}`,
      id: `sha256:${"6".repeat(64)}`,
    },
  },
  artifacts: {
    sourceSha256: "1".repeat(64),
    distSha256: "2".repeat(64),
    toolsSourceSha256: "8".repeat(64),
    toolsDistSha256: "9".repeat(64),
    genesisSha256: "3".repeat(64),
    configSha256: "4".repeat(64),
    acceptanceEnvSha256: "5".repeat(64),
    composeSha256: "6".repeat(64),
    phase4AssetsSha256: "7".repeat(64),
    phasRegistrationProofSha256: isolation.snapshotPhasRegistrationProofSha256,
  },
  phasRegistration: isolation.snapshotPhasRegistration,
  cardanoTip: isolation.snapshotCardanoTip,
  kupoCheckpoint: isolation.snapshotKupoCheckpoint,
});

describe("Phase 4 process isolation", () => {
  it("keeps the fixed transfer below the configured wallet-B genesis total", () => {
    expect(PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE).toBe(50_000n);
    expect(PHASE4_PROCESS_DEFAULT_TRANSFER_LOVELACE).toBeLessThanOrEqual(
      126_943n,
    );
  });

  it("builds a frozen child env without mutable controller leakage", () => {
    const child = buildPhase4IsolatedChildEnv({
      values: { ...values(), EXPLICIT_PHASE4_VALUE: "kept" },
      deploymentManifestPath: isolation.deploymentManifestPath,
      baseEnv: {
        PATH: "/usr/bin",
        TMPDIR: "/tmp",
        BLOCKFROST_PROJECT_ID: "must-not-leak",
        POSTGRES_DB: "live-must-not-leak",
      },
    });
    expect(child).toMatchObject({
      PATH: "/usr/bin",
      TMPDIR: "/tmp",
      POSTGRES_DB: values().POSTGRES_DB,
      EXPLICIT_PHASE4_VALUE: "kept",
      MIDGARD_DEPLOYMENT_MANIFEST_PATH: isolation.deploymentManifestPath,
    });
    expect(child.BLOCKFROST_PROJECT_ID).toBeUndefined();
    expect(Object.isFrozen(child)).toBe(true);
  });

  it("accepts only the run-scoped loopback identity", () => {
    expect(validatePhase4ProcessIsolationValues(values())).toMatchObject({
      postgresPort: 5544,
      ogmiosPort: 2337,
      kupoPort: 2442,
      postgresDatabase: "midgard_phase4_process_test",
    });
  });

  it.each([
    [{ POSTGRES_PORT: "5433" }, "protected live port"],
    [{ L1_OGMIOS_KEY: "http://127.0.0.1:1337" }, "protected live port"],
    [{ L1_KUPO_KEY: "http://127.0.0.1:1442" }, "protected live port"],
    [{ POSTGRES_DB: "midgard" }, "must start with"],
    [{ L1_KUPO_KEY: "https://example.com:2442" }, "loopback"],
    [{ MIN_FEE_A: "10" }, "MIN_FEE_A=0"],
    [{ MIN_FEE_B: "10" }, "MIN_FEE_A=0"],
    [{ MIDGARD_DOTENV_MODE: "enabled" }, "disable checkout dotenv"],
    [{ TESTNET_GENESIS_WALLET_SEED_PHRASE_C: "" }, "missing"],
  ])("rejects protected identity %#", (override, message) => {
    expect(() =>
      validatePhase4ProcessIsolationValues({ ...values(), ...override }),
    ).toThrow(message);
  });

  it("accepts a fully bound reset attestation", () => {
    expect(
      validatePhase4ResetAttestation({
        output: JSON.stringify(attestation()),
        scenarioLabel: attestation().scenarioLabel,
        isolation,
      }),
    ).toEqual(attestation());
  });

  it("decodes exact reset, snapshot, and PHAS V1 shapes only", () => {
    expect(decodePhase4ResetAttestationV1(attestation())).toEqual(
      attestation(),
    );
    expect(decodePhase4MatchedSnapshotIdentityV1(snapshotIdentity())).toEqual(
      snapshotIdentity(),
    );
    expect(
      validatePhase4PhasRegistrationProof(
        isolation.snapshotPhasRegistration,
        "test PHAS proof",
      ),
    ).toEqual(isolation.snapshotPhasRegistration);

    for (const mutation of [
      { ...attestation(), unknown: true },
      {
        ...attestation(),
        cardanoTip: { ...attestation().cardanoTip, unknown: true },
      },
      {
        ...attestation(),
        phasRegistration: {
          ...attestation().phasRegistration,
          transactionBody: {
            ...attestation().phasRegistration.transactionBody,
            certificate: {
              ...attestation().phasRegistration.transactionBody.certificate,
              unknown: true,
            },
          },
        },
      },
      {
        ...attestation(),
        schemaVersion: "midgard-phase4-matched-snapshot-identity-v1",
      },
    ]) {
      expect(() => decodePhase4ResetAttestationV1(mutation)).toThrow();
    }
    const { snapshotSetSha256: _snapshotSetSha256, ...missingResetKey } =
      attestation();
    expect(() => decodePhase4ResetAttestationV1(missingResetKey)).toThrow(
      "fields",
    );

    for (const mutation of [
      { ...snapshotIdentity(), unknown: true },
      {
        ...snapshotIdentity(),
        images: {
          ...snapshotIdentity().images,
          ogmios: { ...snapshotIdentity().images.ogmios, unknown: true },
        },
      },
      {
        ...snapshotIdentity(),
        artifacts: {
          ...snapshotIdentity().artifacts,
          sourceSha256: "A".repeat(64),
        },
      },
      {
        ...snapshotIdentity(),
        schemaVersion: "midgard-phase4-local-devnet-reset-attestation-v1",
      },
    ]) {
      expect(() => decodePhase4MatchedSnapshotIdentityV1(mutation)).toThrow();
    }
  });

  it("accepts only the exact submitted PHAS registration transaction body", () => {
    expect(
      validatePhase4PhasRegistrationTransactionBody(
        PHAS_REGISTRATION_TRANSACTION_BODY,
        isolation.snapshotPhasRegistration,
      ),
    ).toEqual(PHAS_REGISTRATION_TRANSACTION_BODY);

    expect(() =>
      validatePhase4PhasRegistrationTransactionBody(
        {
          ...PHAS_REGISTRATION_TRANSACTION_BODY,
          cborHex: PHAS_REGISTRATION_TRANSACTION_BODY.cborHex.replace(
            PHAS_SCRIPT_HASH,
            "0".repeat(56),
          ),
        },
        isolation.snapshotPhasRegistration,
      ),
    ).toThrow("exact submitted script registration certificate");
  });

  it("rejects witness-bearing CBOR even when its body hash is the submitted transaction", () => {
    const unsigned = CML.Transaction.from_cbor_hex(
      PHAS_REGISTRATION_TRANSACTION_BODY.cborHex,
    );
    const witnessSet = CML.TransactionWitnessSet.new();
    const nativeScripts = CML.NativeScriptList.new();
    nativeScripts.add(
      CML.NativeScript.new_script_all(CML.NativeScriptList.new()),
    );
    witnessSet.set_native_scripts(nativeScripts);
    const witnessedCborHex = CML.Transaction.new(
      unsigned.body(),
      witnessSet,
      true,
      undefined,
    ).to_canonical_cbor_hex();
    const witnessedBytes = Buffer.from(witnessedCborHex, "hex");

    expect(() =>
      validatePhase4PhasRegistrationTransactionBody(
        {
          ...PHAS_REGISTRATION_TRANSACTION_BODY,
          cborHex: witnessedCborHex,
        },
        {
          ...isolation.snapshotPhasRegistration,
          transactionBody: {
            ...isolation.snapshotPhasRegistration.transactionBody,
            cborSha256: createHash("sha256")
              .update(witnessedBytes)
              .digest("hex"),
            cborSizeBytes: witnessedBytes.length,
          },
        },
      ),
    ).toThrow("exact submitted script registration certificate");
  });

  it.each([
    [{ scenarioLabel: "wrong" }, "scenarioLabel mismatch"],
    [{ composeProject: "wrong" }, "noncanonical"],
    [{ snapshotSetSha256: "short" }, "snapshotSetSha256"],
    [{ snapshotIdentitySha256: "short" }, "snapshotIdentitySha256"],
    [{ phasRegistrationProofSha256: "short" }, "phasRegistrationProofSha256"],
    [
      {
        phasRegistration: {
          ...isolation.snapshotPhasRegistration,
          registered: false,
        },
      },
      "read-only registration proof",
    ],
    [
      {
        phasRegistration: {
          ...isolation.snapshotPhasRegistration,
          rewardAddress:
            "stake_test1uz6h73c2gdpu3sawh5mk0g9f5q4x0shz8xq9m8g9rj7k6ssyqg4qy",
        },
      },
      "canonical deployed PHAS identity",
    ],
    [
      {
        phasRegistration: {
          ...isolation.snapshotPhasRegistration,
          transactionBody: {
            ...isolation.snapshotPhasRegistration.transactionBody,
            cardanoCliTxHash: "f".repeat(64),
          },
        },
      },
      "cardano-cli-inspected",
    ],
    [{ kupoCheckpoint: 1233 }, "must equal"],
  ])("rejects unbound reset evidence %#", (override, message) => {
    expect(() =>
      validatePhase4ResetAttestation({
        output: JSON.stringify({ ...attestation(), ...override }),
        scenarioLabel: attestation().scenarioLabel,
        isolation,
      }),
    ).toThrow(message);
  });

  it("rejects reset logs around the JSON", () => {
    expect(() =>
      validatePhase4ResetAttestation({
        output: `resetting\n${JSON.stringify(attestation())}`,
        scenarioLabel: attestation().scenarioLabel,
        isolation,
      }),
    ).toThrow("exactly one JSON attestation");
  });

  it("rejects the same frozen slot with a different Cardano hash", () => {
    expect(() =>
      validatePhase4ResetAttestation({
        output: JSON.stringify({
          ...attestation(),
          cardanoTip: {
            ...attestation().cardanoTip,
            hash: "e".repeat(64),
          },
          phasRegistration: {
            ...attestation().phasRegistration,
            observedAtTip: {
              ...attestation().phasRegistration.observedAtTip,
              hash: "e".repeat(64),
            },
          },
        }),
        scenarioLabel: attestation().scenarioLabel,
        isolation,
      }),
    ).toThrow("does not match the frozen snapshot proof");
  });

  it("rejects a delayed Kupo checkpoint until it reaches the frozen slot", () => {
    expect(() =>
      validatePhase4ResetAttestation({
        output: JSON.stringify({
          ...attestation(),
          kupoCheckpoint: isolation.snapshotKupoCheckpoint - 1,
        }),
        scenarioLabel: attestation().scenarioLabel,
        isolation,
      }),
    ).toThrow("must equal");
  });
});
