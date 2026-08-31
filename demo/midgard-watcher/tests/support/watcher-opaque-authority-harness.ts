import {
  createHash,
  createPrivateKey,
  createPublicKey,
  sign,
  X509Certificate,
} from "node:crypto";
import { type Server } from "node:net";
import { createServer as createTlsServer } from "node:tls";

import { validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../../midgard-core/src/consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "../../../midgard-core/src/da-transport.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  makeDeploymentMarkerV1,
} from "../../../midgard-core/src/deployment-manifest-identity-v1.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/config.js";
import {
  makeWatcherDeploymentIdentitySignaturePayloadV1,
  verifyWatcherDeploymentIdentityV1,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicyV1,
} from "../../src/deployment-identity.js";
import { makeWatcherFinalityPolicyV1 } from "../../src/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  establishWatcherExternalProviderTransportV1,
  normalizeWatcherL1BlockV1 as normalizeRaw,
  type WatcherAuthenticatedL1ProviderV1,
  type WatcherL1TransportAttestationContextV1,
  watcherL1TransportAttestationDetailsV1,
} from "../../src/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 as consistencyRaw } from "../../src/multi-provider-consistency.js";
import { canonicalFraudProofCatalogueFixture } from "../canonical-fraud-proof-catalogue.js";

type AuthorityContractFixtureV1 = Readonly<{
  refScriptUTxO: Readonly<{ txHash: string; outputIndex: number }> | null;
  contract: Readonly<{ type: string; cborHex: string }>;
  scriptHash: string;
}> & {
  fraudProofCatalogue?: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
};
type AuthorityReferenceScriptFixtureV1 = Readonly<{
  status: string;
  roleUnit: string;
  scriptHash: string;
  outRef: string;
}>;

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const RELEASE_DIGEST = h32("22");
const BLUEPRINT_HASH = h32("55");
const RULE_BUNDLE_COMMITMENT = h32("44");
const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

// These are test-only, fixed localhost credentials copied from rollback-engine.test.
// Binding an ephemeral port remains deliberately outside every asserted digest.
const TLS_IDENTITY = {
  key: `-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDCa2UwmGuBrfro
QGDYBi79Uq8ICMlsKQaCrK7QVy/ZGDNab7zUmlrwpatn0Tihsb6+S1JqP9cJaI3A
WDaZQl73dpB+DcpnqMuAF0jKMmsedDPPfBD1/zntzn0JuPLP0yw9DqM4BLYdpfNk
JqlDZTuKcMTdNnUUAztew8WYTWANhTsc3FbWRO0+JNuNXnOpJuU1VsSxnmdivNbp
JF6Yt94D/x3tt3vAS10HLwfMbWdZDK346TuBKkrDhN7uPwrP94lm09Ph20IzAnSe
BKG4eekEdnYRSs3Fx7MH3HfvpSNPkgcdnUDaO2k2ZAmrSiWWyv4dSMdKwCNuMZif
HM0hGgeJAgMBAAECggEAEAk+8VNPIcUFkjDWNBdNeqpeYsujvorDPVXMPQXF/fJ9
sN7QzNn2+Iy3rsJeeRLRxH0uvPILVPy1XXlBNqK3ddKnICiXynVNJMF26Ouf84T6
7YkiloHQ58UtgdbqGzuENXyOuK0Fzvv8T4VTVopj7vs2d7cZUNdj5yD/fDycmLzA
2fH0yKlVimz2ojNspl5GKLxjTXri4SMmsO/+kW7FTGGBe4BsQI5OXTRCfLYSHG9j
XuTBkXs7n575aiDHUHNMMQvjBhDgLuC+v2zT0etbEn9+R+FWJYE8alRK0qLWhAnG
SAK7rWyA/EFXuG2x7mMVPuqrtk+LXBgPOOI926lEkQKBgQDfZrtpoxGACNAUA5Hg
nVAkiEnAmfk0pSPHYTZJbVch6QbNWswe94Ge8LnAFaQUBb86ALcIoCJXo8kI7Q/m
8ngx2GQ/j6hqKjukbi/o7sJuDyEgwIdPuVfBglDqHkKQEi4vRiFclTClrZkqOhZe
MfFE7dqfOX3+DEsegccmASn2GQKBgQDeygi3Otr/3rH+vnz3dIcKsjj033zI5Opd
ZrYccT6dO4gx7tD40wO/hRMmq9mSYeURNu8BX2dV8w18Sq/C7h7oiGNYiKQ4GxuK
73MzL1/VLrFUtLCGMh2+1WYWJ0LlsjJhWCAk7gHPvxKuP3x35MOTwrS+WOktAaYa
N5iqNqtq8QKBgQDejJHwx2EcoirfdTryfuSisB6AvyKyHj0JVz9kYIdnoaOEGYq0
4q3/LyJsR2LAC4WXe7Ta4+OyWNhhiv/Hew7f4QjlBPCqak4mHRqfOpL4XxwKa6Gg
eyv/+xkuUVzP9zyJHZ0IhRsEQW8O0PUNe0U1/JlI+1YXKhn/VxuUMZ6iqQKBgAzi
RirCfpO5fzWqMnPlC0I1GFIg8ohzpJIONI3khqh1HuU0WGVrXpYezgK4gXaTrrmW
IbBEoic4TRlZAF0XhDYSXRxrmoOcHbWlL1ZQcQxVDPBHGsZH86xrjuHNF3NNINi8
Te+UzAoFlMD67unIEv9ijS1M2v89TyvI900wqC0hAoGBAJmXWF58v5aE2aHMHv+W
JDrwZVdqUKEz0t+5cUXmeOHASm381w+/2N9TQMO42Wog2bootXRqOKxtyTiY7YEx
kk0BS+e78RmcOnO3lWH+6oKVo+OmlX5JsX5x9WpcFuPLn6UjERJ1Y/qGNnOuh+Iu
/7Q4gPY3+xgXmhmSWVzhKEJG
-----END PRIVATE KEY-----`,
  cert: `-----BEGIN CERTIFICATE-----
MIIDHzCCAgegAwIBAgIUActK3rYJ7ivz27sB4pIdx7IlsPgwDQYJKoZIhvcNAQEL
BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDczMDA1NDkwN1oXDTM2MDcy
NzA1NDkwN1owFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEAwmtlMJhrga366EBg2AYu/VKvCAjJbCkGgqyu0Fcv2Rgz
Wm+81Jpa8KWrZ9E4obG+vktSaj/XCWiNwFg2mUJe93aQfg3KZ6jLgBdIyjJrHnQz
z3wQ9f857c59Cbjyz9MsPQ6jOAS2HaXzZCapQ2U7inDE3TZ1FAM7XsPFmE1gDYU7
HNxW1kTtPiTbjV5zqSblNVbEsZ5nYrzW6SRemLfeA/8d7bd7wEtdBy8HzG1nWQyt
+Ok7gSpKw4Te7j8Kz/eJZtPT4dtCMwJ0ngShuHnpBHZ2EUrNxcezB9x376UjT5IH
HZ1A2jtpNmQJq0ollsr+HUjHSsAjbjGYnxzNIRoHiQIDAQABo2kwZzAdBgNVHQ4E
FgQUAOQ/IIFQCNpxyO4uB++rl27U9HUwHwYDVR0jBBgwFoAUAOQ/IIFQCNpxyO4u
B++rl27U9HUwDwYDVR0TAQH/BAUwAwEB/zAUBgNVHREEDTALgglsb2NhbGhvc3Qw
DQYJKoZIhvcNAQELBQADggEBALvUTrMsAhwWOdLWB/EDvsxer1tTzIyJRns7PwPU
rMEratP19KsbxnbIqbFD4379AE5RjudIN4+q5Guocg0GrATOiKBD5H7I9umsMRVI
JCirdYP/l+9uWr4c7BToaRdWEZ0+Jqn34aLA9Dv2hX5Pt+X7A4srdr6zR2Vw/D8o
B1uO1VwDosNAJsTmXQ6Su33klvVZE0awLyG+esxey7XUtysdXKeh47MgiRshIwyR
74KDBj95x3C5nPVtL1yGRhaJy7S4yVzP6b1a7ctoR4/xotikVNeyL1FoQTeuzq2O
1/G1W3LM8WYCREXQRuIdr+F5D0vogZqVCnfEQBp+/vbtcYU=
-----END CERTIFICATE-----`,
} as const;

const listen = async (server: Server): Promise<number> =>
  await new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.off("error", reject);
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(
          new Error("opaque authority TLS fixture did not bind a TCP port"),
        );
        return;
      }
      resolve(address.port);
    });
  });

const closeServer = async (server: Server): Promise<void> =>
  await new Promise((resolve, reject) =>
    server.close((error) => (error ? reject(error) : resolve())),
  );

const fixedEd25519Key = () =>
  createPrivateKey({
    key: Buffer.concat([
      Buffer.from("302e020100300506032b657004220420", "hex"),
      Buffer.alloc(32, 0x25),
    ]),
    format: "der",
    type: "pkcs8",
  });

const createWatcherAuthorityDeploymentFixtureV1 = () => {
  const referenceOutRefByContract = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: h32("12"), outputIndex },
      ],
    ),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName, index) => {
      const native = contractName === "referenceScriptAuthMint";
      const script = native
        ? NATIVE_SCRIPT_CBOR
        : (index + 1).toString(16).padStart(2, "0");
      return [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          contract: { type: native ? "Native" : "PlutusV3", cborHex: script },
          scriptHash: native
            ? NATIVE_SCRIPT_HASH
            : validatorToScriptHash({ type: "PlutusV3", script }),
        },
      ];
    }),
  ) as Record<string, AuthorityContractFixtureV1>;
  const fraudProofCatalogue = canonicalFraudProofCatalogueFixture(contracts);
  const catalogueContract = contracts.fraudProofCatalogueMint;
  if (catalogueContract === undefined) {
    throw new Error("authority catalogue contract is missing");
  }
  catalogueContract.fraudProofCatalogue = fraudProofCatalogue;
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => {
      const outRef = referenceOutRefByContract.get(contractName)!;
      const tokenName =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
        ];
      return [
        role,
        {
          status: "confirmed",
          roleUnit:
            NATIVE_SCRIPT_HASH + Buffer.from(tokenName, "utf8").toString("hex"),
          scriptHash: contracts[contractName].scriptHash,
          outRef: `${outRef.txHash}#${outRef.outputIndex}`,
        },
      ];
    }),
  ) as Record<string, AuthorityReferenceScriptFixtureV1>;
  const parameters = {
    minFeeA: "44",
    minFeeB: "155381",
    priceMemory: { numerator: "577", denominator: "10000" },
    priceSteps: { numerator: "721", denominator: "10000000" },
    coinsPerUtxoByte: "4310",
    collateralPercentage: "150",
    maxCollateralInputs: "3",
    maxTxSize: "16384",
    maxValueSize: "5000",
    maxTxExUnits: { memory: "16500000", steps: "10000000000" },
    referenceScriptFee: {
      base: { numerator: "15", denominator: "1" },
      range: "25600",
      multiplier: { numerator: "6", denominator: "5" },
      maximumSizeBytes: "204800",
    },
  };
  const hubOracleOneShot = {
    txHash: h32("11"),
    outputIndex: 0,
    outRef: `${h32("11")}#0`,
    status: "consumed_by_init",
  };
  const daIdentity = {
    committeeVkeys: [h32("44")],
    committeeSignersHash: DA_SIGNERS_HASH,
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
      envelopeEncoding: "identity",
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS_V1,
      retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
    },
  };
  const identity = {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestV1JsonDigest(parameters),
    },
    genesis: {
      headerHash: h28("00"),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest([]),
    },
    createdAt: "2026-07-28T00:00:00.000Z",
    updatedAt: "2026-07-28T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot,
    referenceScriptAuthPolicy: {
      policyId: NATIVE_SCRIPT_HASH,
      nativeScript: {
        type: "Native",
        cborHex: NATIVE_SCRIPT_CBOR,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
        timelockDurationMs: 1,
      },
      tokenNames: DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: daIdentity,
    proofEvidence: { digest: RELEASE_DIGEST, blueprintHash: BLUEPRINT_HASH },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_STEP_NAMES.map((name) => [
        name,
        {
          status:
            name === "prepareHubOracleNonce" ||
            name === "deployNodeRuntimeReferenceScripts" ||
            name === "initProtocol"
              ? "complete"
              : "pending",
        },
      ]),
    ),
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
    },
    economics:
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    l1Finality: DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
    availabilityChallenge: {
      responseClasses: {
        smallPayloadMaxBytes: 65_536,
        smallResponseWindowMs: 3_600_000,
        fullPayloadMaxBytes: 67_108_864,
        fullResponseWindowMs: 172_800_000,
      },
      responseGeometry: {
        chunkByteLength: 14_020,
        trancheByteLength: 4_194_304,
        maxTrancheCount: 16,
      },
      daBondLovelace: 10_000_000_000,
      challengerBondLovelace: 10_000_000_000,
      maxOpenFeeLovelace: 500_000,
      maxPublicationFeeLovelace: 500_000,
      maxSettlementFeeLovelace: 500_000,
      maxCloseFeeLovelace: 1_000_000,
      maxTimeoutFeeLovelace: 1_200_000,
      bondOwnerCredential: h28("77"),
    },
  };
  const manifestId = computeDeploymentManifestV1Id(identity);
  const manifest = {
    ...identity,
    manifestId,
  };
  const programCommitments = {
    "validation-machine-v1": h32("88"),
    "transition-order-v1": h32("99"),
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestV1JsonDigest(daIdentity),
    },
    releaseEvidence: { digest: RELEASE_DIGEST, blueprintHash: BLUEPRINT_HASH },
  };
  const privateKey = fixedEd25519Key();
  const publicKeySpkiDerHex = createPublicKey(privateKey)
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");
  const signedIdentity = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: { algorithm: "ed25519", trustRootId, signature: "" },
  };
  signedIdentity.attestation.signature = sign(
    null,
    makeWatcherDeploymentIdentitySignaturePayloadV1(
      manifestId,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const policy: WatcherDeploymentIdentityPolicyV1 = {
    network: "Preprod",
    hubOracleOneShotOutRef: hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((name) => [
        name,
        contracts[name].scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (role) => [
          role,
          {
            scriptHash: referenceScripts[role]!.scriptHash,
            outRef: referenceScripts[role]!.outRef,
          },
        ],
      ),
    ),
    fraudProofCatalogue: {
      root: fraudProofCatalogue.root,
      categories: Object.fromEntries(
        Object.entries(fraudProofCatalogue.categories).map(([name, value]) => {
          const category = value as {
            readonly categoryId?: unknown;
            readonly scriptHash?: unknown;
          };
          if (
            typeof category.categoryId !== "string" ||
            typeof category.scriptHash !== "string"
          ) {
            throw new Error("authority catalogue category is malformed");
          }
          return [
            name,
            {
              categoryId: category.categoryId,
              scriptHash: category.scriptHash,
            },
          ];
        }),
      ),
    } as WatcherDeploymentIdentityPolicyV1["fraudProofCatalogue"],
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    releaseEvidenceDigest: RELEASE_DIGEST,
    blueprintHash: BLUEPRINT_HASH,
  };
  const trustRoots = [{ trustRootId, publicKeySpkiDerHex }];
  const marker = makeDeploymentMarkerV1(manifestId);
  const result = verifyWatcherDeploymentIdentityV1({
    signedIdentity,
    policy,
    trustRoots,
    durableMarker: marker,
  });
  if (result === null)
    throw new Error("fixed authority deployment identity was rejected");
  return Object.freeze({
    signedIdentity,
    policy,
    trustRoots,
    result,
    marker,
    contracts,
    applied: policy.appliedScriptHashes,
    authority: Object.freeze({ signedIdentity, policy, trustRoots, result }),
  });
};

export const makeWatcherAuthorityDeploymentFixtureV1 =
  createWatcherAuthorityDeploymentFixtureV1;

export type WatcherOpaqueAuthorityHarnessV1 = Readonly<{
  deploymentFixture: ReturnType<typeof makeWatcherAuthorityDeploymentFixtureV1>;
  transportAttestations: readonly WatcherL1TransportAttestationContextV1[];
  providers: readonly WatcherAuthenticatedL1ProviderV1[];
  externalSource: Readonly<{
    sourceMode: "external_providers";
    network: "Preprod";
    providers: readonly {
      providerId: string;
      operatorIdentitySha256: string;
      endpoint: string;
    }[];
  }>;
  finalityPolicy: NonNullable<ReturnType<typeof makeWatcherFinalityPolicyV1>>;
  normalize: (
    provider: WatcherAuthenticatedL1ProviderV1,
    observation: unknown,
    session?: Parameters<typeof normalizeRaw>[2],
  ) => ReturnType<typeof normalizeRaw>;
  consistency: (
    observations: readonly unknown[],
  ) => ReturnType<typeof consistencyRaw>;
  dispose: () => Promise<void>;
}>;

export const createWatcherOpaqueAuthorityHarnessV1 =
  async (): Promise<WatcherOpaqueAuthorityHarnessV1> => {
    const deploymentFixture = makeWatcherAuthorityDeploymentFixtureV1();
    const servers = [
      createTlsServer(TLS_IDENTITY),
      createTlsServer(TLS_IDENTITY),
    ];
    let ports: number[];
    try {
      ports = await Promise.all(servers.map(listen));
    } catch (error) {
      await Promise.allSettled(
        servers
          .filter((server) => server.listening)
          .map(async (server) => await closeServer(server)),
      );
      throw error;
    }
    const configured = ["provider-a", "provider-b"].map(
      (providerId, index) => ({
        providerId,
        operatorIdentitySha256: h32(index === 0 ? "97" : "98"),
        endpoint: `https://localhost:${ports[index]!}`,
      }),
    );
    const transportAttestations: WatcherL1TransportAttestationContextV1[] = [];
    let disposed = false;
    const disposeHarness = async (): Promise<void> => {
      if (disposed) return;
      disposed = true;
      for (const context of transportAttestations) {
        closeWatcherL1TransportAttestationContextV1(context);
      }
      await Promise.allSettled(
        servers.map(async (server) => await closeServer(server)),
      );
    };
    try {
      for (const provider of configured) {
        transportAttestations.push(
          await establishWatcherExternalProviderTransportV1({
            network: "Preprod",
            providerId: provider.providerId,
            operatorIdentitySha256: provider.operatorIdentitySha256,
            endpoint: provider.endpoint,
            caPem: TLS_IDENTITY.cert,
            expectedTlsPublicIdentitySha256: createHash("sha256")
              .update(new X509Certificate(TLS_IDENTITY.cert).raw)
              .digest("hex"),
            connectTimeoutMs: 2_000,
          }),
        );
      }
    } catch (error) {
      await disposeHarness();
      throw error;
    }
    const providers = transportAttestations
      .map(
        (context) => watcherL1TransportAttestationDetailsV1(context)?.provider,
      )
      .filter(
        (provider): provider is WatcherAuthenticatedL1ProviderV1 =>
          provider !== null,
      );
    if (providers.length !== 2) {
      await disposeHarness();
      throw new Error("opaque authority harness did not establish providers");
    }
    const externalSource = Object.freeze({
      sourceMode: "external_providers" as const,
      network: "Preprod" as const,
      providers: Object.freeze(configured),
    });
    const finalityPolicy = makeWatcherFinalityPolicyV1(
      {
        schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
        mode: "development",
        targetNetwork: "Preprod",
        l1: {
          source: {
            sourceMode: "external_providers",
            providers: configured.map(
              ({ providerId, operatorIdentitySha256, endpoint }) => ({
                identity: providerId,
                operatorIdentitySha256,
                endpoint,
              }),
            ),
          },
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
          finality: {
            depth: 2,
            rollback: {
              beforeFinality: "rewind",
              afterFinality: "quarantine",
              maxDepth: 2,
            },
          },
        },
        da: {
          peers: [
            {
              identity: "da-peer-a",
              multiaddr:
                "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
            },
          ],
          requestTimeoutMs: 10_000,
          maxConcurrency: 4,
        },
        storage: {
          driver: "sqlite",
          path: "/var/lib/midgard-watcher/watcher.sqlite",
          rollbackAuthorityKeySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
          },
        },
        proverWallet: {
          keySource: {
            kind: "environment",
            variable: "MIDGARD_WATCHER_PROVER_KEY",
          },
        },
        deadlines: {
          daFetchMs: 60_000,
          daPublishMs: 60_000,
          proofConstructMs: 300_000,
          proofSubmitMs: 120_000,
        },
      },
      {
        manifestId: deploymentFixture.marker.manifestId,
        network: "Preprod",
        trustRootId: deploymentFixture.result.trustRootId,
        releaseEvidenceDigest: RELEASE_DIGEST,
        ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
        programCommitments: { validation: h32("55") },
        durableMarker: deploymentFixture.marker,
      },
    );
    if (finalityPolicy === null) {
      await disposeHarness();
      throw new Error("opaque authority harness finality policy was rejected");
    }
    const transportFor = (provider: WatcherAuthenticatedL1ProviderV1) => {
      const matches = transportAttestations.filter(
        (context) =>
          watcherL1TransportAttestationDetailsV1(context)?.provider
            .providerId === provider.providerId,
      );
      if (matches.length !== 1)
        throw new Error(
          "live opaque transport context is missing or ambiguous",
        );
      return matches[0]!;
    };
    const provenance = new WeakMap<
      object,
      WatcherL1TransportAttestationContextV1
    >();
    return Object.freeze({
      deploymentFixture,
      transportAttestations: Object.freeze(transportAttestations),
      providers: Object.freeze(providers),
      externalSource,
      finalityPolicy,
      normalize: (provider, observation, session) => {
        const normalized = normalizeRaw(
          transportFor(provider),
          observation,
          session,
        );
        provenance.set(normalized, transportFor(provider));
        return normalized;
      },
      consistency: (observations) =>
        consistencyRaw(
          externalSource,
          observations,
          observations.map((observation) => {
            const transport =
              typeof observation === "object" && observation !== null
                ? provenance.get(observation)
                : undefined;
            if (transport === undefined)
              throw new Error(
                "observation has no live opaque transport provenance",
              );
            return transport;
          }),
        ),
      dispose: disposeHarness,
    });
  };
