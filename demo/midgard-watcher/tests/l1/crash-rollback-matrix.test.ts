import { createHash, X509Certificate } from "node:crypto";
import { type Server } from "node:net";
import { createServer as createTlsServer } from "node:tls";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import {
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  makeDeploymentMarkerV1,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateWatcherFinalityV1,
  makeWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityStateV1,
} from "../../src/l1/finality-engine.js";
import {
  closeWatcherL1TransportAttestationContextV1,
  encodeWatcherNormalizedL1BlockV1,
  establishWatcherExternalProviderTransportV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  type WatcherL1TransportAttestationContextV1,
  type WatcherNormalizedL1BlockV1,
} from "../../src/l1/l1-adapter.js";
import { evaluateWatcherMultiProviderConsistencyV1 } from "../../src/l1/multi-provider-consistency.js";
import {
  evaluateAndPersistWatcherPostFinalityRecoveryV1,
  evaluateAndPersistWatcherRollbackV1,
  evaluateWatcherRollbackV1,
  initializeWatcherRollbackDurableAuthorityV1,
  loadWatcherRollbackDurableAuthorityV1,
  makeWatcherRollbackBootstrapStateV1,
  WATCHER_ROLLBACK_V1_BOUNDS,
  watcherRollbackDurableAuthorityStatusV1,
  type WatcherRollbackRemovedRecordsV1,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  compareAndSwapWatcherDurableAtomicSnapshotV1,
  decodeWatcherDurableStoreV1,
  encodeWatcherDurableStoreV1,
  makeWatcherDurablePayloadV1,
  makeWatcherDurableStoreV1,
  migrateWatcherDurableStoreV1,
  readWatcherDurableAtomicSnapshotV1,
  type WatcherDurableAtomicBackend,
  type WatcherDurableRecordsV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
} from "../../src/storage/durable-store.js";

/*
 * W44 crash/rollback matrix.
 *
 * The matrix is exactly 17 cases:
 *   - 14 before/after crash points across the 7 W32 durable lifecycle
 *     transitions (detect, persist evidence, init, steps, proof token,
 *     removal/slashing, terminal verification);
 *   -  1 ordinary pre-finality L1 rollback;
 *   -  1 rollback deeper than the configured finality depth and within the
 *      fixed Cardano k = 2160 automated recovery bound;
 *   -  1 configured-source inconsistency.
 *
 * Every case asserts the four W44 zero-defect invariants (0 double submits,
 * 0 lost evidence, 0 false verified states, 0 unrecoverable workflows) plus
 * the four IG3 watcher security conditions (public data only, configured
 * source consistency, maturity budget, enabled family) and that readiness is
 * never true from a partially recovered state.
 *
 * There is no operator-driven repair step anywhere in this file: recovery is
 * always "restart the same deterministic driver against the same durable
 * backend", which is exactly the "no manual surgery" requirement.
 */

const hex32 = (byte: string): string => byte.repeat(32);

const testTlsIdentities = [
  {
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
  },
  {
    key: `-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC/nPg3UUCGOpKo
JjDOwWMNVS339Rccx1wZRlVuz6KW/rm78GiHNdW/aGs0zDGBCnVcGamvC8dMsBaq
P3E5R6JXGGTIPFWe9zfLEr5Cws27TBFBChKlVqRoukMfDsOxu9XEv+yR+lZzPx04
eJDbNedzPLu3ZPhqv0QRtcBePHSeYFQ/w/9wlGM7HEbsonCA1ydk+6qzdjYxji6D
6SkXnKHuPq+C9Jmull1QwBr0r439YZ2CeKR9oYas6RsflVCsly4GY6V4sO6rI/He
DE3n+G3+gjlIy03k16ODeNoPGG5OU5o0tsK/drZkBozK/h/QP7zKjAnElDC4t2Q4
Vr2Z0UWZAgMBAAECggEAB5wxgAnwNvyNzVOZ+eY0m8eTqC7R5IzW77KLS1fACID0
qb4UOrWEyCG6q0nGWA6NJ3Ol+XutZlJyjf+vzKN3kz+25fx+do4xRzWW/KINx3fv
kf6XS72HkVi/eHTuyQjxpisst0YC57gcnhzsvOYUy48Akhm2o4+14YGvUpbSV2Vg
rFEiEOr3F96pLG9v5kezpWnRpH1eZw+yWVOrR4niq05OkNeleIrNaUb0dBmlPiQg
cxmx1f2uDpG5TewOMFOSvIOfwAtuHxBlZ5Uu5HUgnnDyeqetXvjwouciF/Dbfwm+
4m9V9yQturygcAqlVqfUMlWbfRJnK7CAXMzbDgwSQQKBgQDl5zxFmdqDvvlg0E4u
yxGJTM4eyVlXvhpolt/nOVfpdiXUU/IvvlnHB8/SG5Hn/gSkWSGo9YC9kasnMXam
GAp+Toewut6qh86NDKM4lTHzl3mJ4wVNMk7iPSy4+OSXeWmLZVJnJaUTGS6EF3F/
Rv5og24mU33qPaPhSMt5bBSoWQKBgQDVXQ+pNk9B6lcYaJ2LtiF8fuLhBVGJAGgX
HSQldfL4D1qOYUihqUwkxym5eAtPDNA/Aaox/ReyDhi5+UWj16lvpSMAkGnvNgGv
tzpLXdzVZYoy8ldRAAFveQulpY53FU/DIm3lQpckmQL476J5JAvHQr89xgAu/W0I
L7XE27XfQQKBgBeQLKhBjZjlMPAQSYMYQxLccV/MaUDJ9jD0DbzILs950YTCmdb0
3oS8szsoojqx2U3y6LVFfE1xqaYZtrxtSF4LtHKTpJC73JquSehZukXqJ4XPY9K2
rkkX1gabU+qGgh/MYba6sAGWGiNlt7dA0oBpwBdjhUtFyA8mA9zNDAz5AoGBAKMb
RUGiFuzY7EPolaecT/UQOviyTCZjfS9OQ7evd1JSynNVw2RyO5dR+X+jWWHQ9dF0
wFr+lAK17AkfmjEqSIjkwOFJhPItYxSlCZdb5dnsib1wrXdqfa5t5o13BnXagOM3
irNcOJbtsewDpTzeZXKqf/AFUVaavaModdhL7bkBAoGAc5f7weSFTxC+CmaukDvO
KPw6exu4huhz0ONsXDOMm0L6TdWf2Hi8FxuqOerGmomexqd0j+WWG8dmXOF7U3bf
y1WpXSLsbv5E+NI0qMMErLOG85o2a1XT+a1nml/C1BtL8c8kQIOuib6U9MI3Yh4j
ephjXui/3SIeg9AIPtQPI+w=
-----END PRIVATE KEY-----`,
    cert: `-----BEGIN CERTIFICATE-----
MIIDHzCCAgegAwIBAgIUDjle79EwfzLjdJaGrDr+N7PoBr8wDQYJKoZIhvcNAQEL
BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDczMDA1NDk0MloXDTM2MDcy
NzA1NDk0MlowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEAv5z4N1FAhjqSqCYwzsFjDVUt9/UXHMdcGUZVbs+ilv65
u/BohzXVv2hrNMwxgQp1XBmprwvHTLAWqj9xOUeiVxhkyDxVnvc3yxK+QsLNu0wR
QQoSpVakaLpDHw7DsbvVxL/skfpWcz8dOHiQ2zXnczy7t2T4ar9EEbXAXjx0nmBU
P8P/cJRjOxxG7KJwgNcnZPuqs3Y2MY4ug+kpF5yh7j6vgvSZrpZdUMAa9K+N/WGd
gnikfaGGrOkbH5VQrJcuBmOleLDuqyPx3gxN5/ht/oI5SMtN5Nejg3jaDxhuTlOa
NLbCv3a2ZAaMyv4f0D+8yowJxJQwuLdkOFa9mdFFmQIDAQABo2kwZzAdBgNVHQ4E
FgQUNESp4o+aYjZ9p2goiZ8RyDQtoj8wHwYDVR0jBBgwFoAUNESp4o+aYjZ9p2go
iZ8RyDQtoj8wDwYDVR0TAQH/BAUwAwEB/zAUBgNVHREEDTALgglsb2NhbGhvc3Qw
DQYJKoZIhvcNAQELBQADggEBALnscJR+cTdQH3XL26q+KE8iE9HUsSH01tjrLD5z
0EQ6jIrG7aBPd2E++N+Plme2sLXR6n5oydCqUle7CARgiIaeLpdNmxuQJK7t68fd
GE9pOiXqxMdwPWelRgjk2LqzNQqBY94aJJNQt9B1i4/0ji2U7rSwr4/RQtqCTRsO
EjzIDJ0dPMi6neBdMtZ1p0VYX2hF1iSZ09Tt/Z91seGAJ46pDSH8eRzmMAhrrTWT
iyHCdKMMt7XpRNcuGUM5kn222xyTbdBZu68qcVABi1U48i2G2pLFQpvUy0rjNu89
9JLubbJMBhdUPBFHRoRgp3wBtsHmpfSUc0AbOyzdomL5/es=
-----END CERTIFICATE-----`,
  },
] as const;

const payload = (
  cborHex = "80",
): ReturnType<typeof makeWatcherDurablePayloadV1> =>
  makeWatcherDurablePayloadV1(cborHex);

const rollbackAuthorityKey = Uint8Array.from(
  { length: 32 },
  (_, index) => index + 1,
);

const canonicalJsonForTest = (value: unknown): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      throw new Error("unsupported test number");
    }
    return value.toString();
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalJsonForTest).join(",")}]`;
  }
  if (typeof value === "object") {
    const record = value as Record<string, unknown>;
    return `{${Object.keys(record)
      .sort()
      .map(
        (key) => `${JSON.stringify(key)}:${canonicalJsonForTest(record[key])}`,
      )
      .join(",")}}`;
  }
  throw new Error("unsupported test value");
};

const sha256Canonical = (value: unknown): string =>
  createHash("sha256")
    .update(canonicalJsonForTest(value), "utf8")
    .digest("hex");

/**
 * A durable backend that can be crashed at an exact compare-and-swap boundary.
 *
 * `crashBeforeAttempt` throws before the bytes are replaced (the transition is
 * lost). `crashAfterAttempt` throws after the bytes are durably replaced (the
 * caller cannot tell whether the transition landed). Both are the ambiguous
 * windows a real process crash produces around a journal boundary.
 */
class CrashInjectingAtomicBackend implements WatcherDurableAtomicBackend {
  bytes: Uint8Array | null = null;
  attempts = 0;
  writes = 0;
  crashBeforeAttempt: number | null = null;
  crashAfterAttempt: number | null = null;

  async read(): Promise<Uint8Array | null> {
    return this.bytes === null ? null : Uint8Array.from(this.bytes);
  }

  async compareAndSwap(
    expectedSha256: string | null,
    next: Uint8Array,
  ): Promise<boolean> {
    const actualSha256 =
      this.bytes === null ? null : watcherDurableStoreBytesSha256(this.bytes);
    if (actualSha256 !== expectedSha256) {
      return false;
    }
    this.attempts += 1;
    if (this.crashBeforeAttempt === this.attempts) {
      this.crashBeforeAttempt = null;
      throw new Error("simulated crash before durable commit");
    }
    this.bytes = Uint8Array.from(next);
    this.writes += 1;
    if (this.crashAfterAttempt === this.attempts) {
      this.crashAfterAttempt = null;
      throw new Error("simulated crash after durable commit");
    }
    return true;
  }

  digest(): string | null {
    return this.bytes === null
      ? null
      : watcherDurableStoreBytesSha256(this.bytes);
  }

  snapshotStore(): WatcherDurableStoreV1 {
    if (this.bytes === null) {
      throw new Error("backend has no durable snapshot");
    }
    return decodeWatcherDurableStoreV1(this.bytes);
  }
}

let watcherTransportFixtureServers: Server[] = [];
let externalProviderATransport: WatcherL1TransportAttestationContextV1;
let externalProviderBTransport: WatcherL1TransportAttestationContextV1;
let externalProviderEndpoints: readonly [string, string] = [
  "https://localhost:1/provider-a",
  "https://localhost:1/provider-b",
];
let watcherTransportAttestations: readonly WatcherL1TransportAttestationContextV1[] =
  [];

const listen = (server: Server, port: number, host: string): Promise<void> =>
  new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, host, () => {
      server.off("error", reject);
      resolve();
    });
  });

const closeServer = (server: Server): Promise<void> =>
  new Promise((resolve, reject) => {
    if (!server.listening) {
      resolve();
      return;
    }
    server.close((error) => {
      if (error === undefined) {
        resolve();
      } else {
        reject(error);
      }
    });
  });

const externalSource = () =>
  ({
    sourceMode: "external_providers",
    network: "Preprod",
    providers: [
      {
        providerId: "provider-a",
        operatorIdentitySha256: hex32("a1"),
        endpoint: externalProviderEndpoints[0],
      },
      {
        providerId: "provider-b",
        operatorIdentitySha256: hex32("b2"),
        endpoint: externalProviderEndpoints[1],
      },
    ],
  }) as const;

const CONFIGURED_PROVIDER_IDS: ReadonlySet<string> = new Set([
  "provider-a",
  "provider-b",
]);

const FINALITY_DEPTH = 5;

/** The §3.3 maturity budget in slots (1 slot per second on Cardano). */
const MATURITY_BUDGET_SLOTS = 604_800n;

/**
 * The enabled proof families, derived from the single deployment-manifest
 * catalogue authority and mapped into the durable `familyId` stable-name form.
 */
const ENABLED_FAMILY_IDS: ReadonlySet<string> = new Set(
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) =>
    category.replace(/[A-Z]/gu, (letter) => `-${letter.toLowerCase()}`),
  ),
);

const config = (depth = FINALITY_DEPTH) => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "development",
  targetNetwork: "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: hex32("a1"),
          endpoint: externalProviderEndpoints[0],
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: hex32("b2"),
          endpoint: externalProviderEndpoints[1],
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 4,
    finality: {
      depth,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: depth,
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
});

const deploymentIdentity = (manifestByte = "11", releaseByte = "22") => ({
  manifestId: hex32(manifestByte),
  network: "Preprod" as const,
  trustRootId: hex32("33"),
  releaseEvidenceDigest: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarkerV1(hex32(manifestByte)),
});

const policy = (): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(config(), deploymentIdentity());
  expect(value).not.toBeNull();
  return value as WatcherFinalityPolicyV1;
};

type Point = Readonly<{
  blockHash: string;
  parentBlockHash?: string | null;
  slot: string;
  blockNo: string;
  depth: string;
  bodyHex?: string;
}>;

const transaction = (seedHex: string) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    BigInt(`0x${seedHex}`),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    true,
    undefined,
  );
  const bodyBytes = body.to_canonical_cbor_hex();
  return {
    txHash: computeHash32(Buffer.from(bodyBytes, "hex")).toString("hex"),
    fullTransaction: makeWatcherL1PublicBytesV1(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytesV1(bodyBytes),
    witnessSet: makeWatcherL1PublicBytesV1(witnessSet.to_canonical_cbor_hex()),
    utxos: [],
    scripts: [],
    datums: [],
    redeemers: [],
  };
};

const observation = (
  providerId: string,
  point: Point,
): WatcherNormalizedL1BlockV1 =>
  normalizeWatcherL1BlockV1(
    providerId === "provider-a"
      ? externalProviderATransport
      : externalProviderBTransport,
    {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
      network: "Preprod",
      providerId,
      chainPoint: {
        blockHash: point.blockHash,
        parentBlockHash: point.parentBlockHash ?? null,
        slot: point.slot,
        blockNo: point.blockNo,
        depth: point.depth,
      },
      transactions:
        point.bodyHex === undefined ? [] : [transaction(point.bodyHex)],
    },
  );

const agreementObservations = (
  point: Point,
): readonly WatcherNormalizedL1BlockV1[] => [
  observation("provider-a", point),
  observation("provider-b", point),
];

const agreement = (point: Point) =>
  evaluateWatcherMultiProviderConsistencyV1(
    externalSource(),
    agreementObservations(point),
    watcherTransportAttestations,
  );

/* ------------------------------------------------------------------------ */
/* The seven W32 durable lifecycle transitions                              */
/* ------------------------------------------------------------------------ */

const W44_LIFECYCLE_TRANSITIONS_V1 = [
  "detect",
  "persist_evidence",
  "init",
  "steps",
  "proof_token",
  "removal_slashing",
  "terminal_verification",
] as const;

type W44LifecycleTransitionV1 = (typeof W44_LIFECYCLE_TRANSITIONS_V1)[number];

const LIFECYCLE_IDS = Object.freeze({
  observation: hex32("01"),
  chainPoint: hex32("02"),
  outRef: `${hex32("03")}#0`,
  input: hex32("04"),
  blockHash: hex32("b0"),
  fault: hex32("05"),
  submission: hex32("06"),
  confirmation: hex32("07"),
  retry: hex32("08"),
  proofDeadline: hex32("09"),
  confirmationDeadline: hex32("0a"),
  correction: hex32("0b"),
});

const LIFECYCLE_POINT: Point = Object.freeze({
  blockHash: hex32("f1"),
  slot: "1000",
  blockNo: "100",
  depth: "6",
});

const LIFECYCLE_FAMILY_ID = "transition-trace";
const LIFECYCLE_REWARD_LOVELACE = "7000000";
const LIFECYCLE_SLASH_LOVELACE = "9000000";

const recordsOf = (store: WatcherDurableStoreV1): WatcherDurableRecordsV1 => ({
  l1Observations: store.l1Observations,
  chainPoints: store.chainPoints,
  protocolUtxos: store.protocolUtxos,
  spentProtocolUtxos: store.spentProtocolUtxos,
  daProofInputs: store.daProofInputs,
  reconstructedStates: store.reconstructedStates,
  decisions: store.decisions,
  faults: store.faults,
  submissions: store.submissions,
  confirmations: store.confirmations,
  retries: store.retries,
  deadlines: store.deadlines,
  correctionResults: store.correctionResults,
});

/**
 * Applies exactly one W32 lifecycle transition to the durable record set.
 * Every transition is a pure function of the prior records, which is what
 * makes crash recovery a replay rather than a repair.
 */
const applyLifecycleTransitionV1 = (
  records: WatcherDurableRecordsV1,
  transition: W44LifecycleTransitionV1,
  publicObservation: WatcherNormalizedL1BlockV1,
): WatcherDurableRecordsV1 => {
  const slot = BigInt(LIFECYCLE_POINT.slot);
  switch (transition) {
    case "detect":
      return {
        ...records,
        chainPoints: [
          ...records.chainPoints,
          {
            chainPointId: LIFECYCLE_IDS.chainPoint,
            providerId: "provider-a",
            blockHash: LIFECYCLE_POINT.blockHash,
            slot: LIFECYCLE_POINT.slot,
            blockNo: LIFECYCLE_POINT.blockNo,
            depth: LIFECYCLE_POINT.depth,
          },
        ],
        l1Observations: [
          ...records.l1Observations,
          {
            observationId: LIFECYCLE_IDS.observation,
            providerId: "provider-a",
            chainPointId: LIFECYCLE_IDS.chainPoint,
            payload: makeWatcherDurablePayloadV1(
              encodeWatcherNormalizedL1BlockV1(publicObservation).toString(
                "hex",
              ),
            ),
          },
        ],
        protocolUtxos: [
          ...records.protocolUtxos,
          {
            outRef: LIFECYCLE_IDS.outRef,
            role: "state_queue",
            chainPointId: LIFECYCLE_IDS.chainPoint,
            output: payload("d87980"),
          },
        ],
      };
    case "persist_evidence":
      return {
        ...records,
        daProofInputs: [
          ...records.daProofInputs,
          {
            inputId: LIFECYCLE_IDS.input,
            kind: "da_payload",
            payload: payload("4401020304"),
          },
        ],
        reconstructedStates: [
          ...records.reconstructedStates,
          {
            blockHash: LIFECYCLE_IDS.blockHash,
            chainPointId: LIFECYCLE_IDS.chainPoint,
            priorStateRoot: hex32("c1"),
            postStateRoot: hex32("c2"),
            inputIds: [LIFECYCLE_IDS.input],
            state: payload("82190100190101"),
          },
        ],
        decisions: [
          ...records.decisions,
          {
            blockHash: LIFECYCLE_IDS.blockHash,
            decision: "fault_detected",
            reconstructionDigest: hex32("c3"),
            evidenceDigest: hex32("c4"),
          },
        ],
        faults: [
          ...records.faults,
          {
            faultId: LIFECYCLE_IDS.fault,
            blockHash: LIFECYCLE_IDS.blockHash,
            familyId: LIFECYCLE_FAMILY_ID,
            evidence: payload("a10001"),
          },
        ],
      };
    case "init":
      return {
        ...records,
        submissions: [
          ...records.submissions,
          {
            submissionId: LIFECYCLE_IDS.submission,
            faultId: LIFECYCLE_IDS.fault,
            txBodyHash: hex32("c5"),
            status: "prepared",
          },
        ],
        deadlines: [
          ...records.deadlines,
          {
            deadlineId: LIFECYCLE_IDS.proofDeadline,
            subjectKind: "submission",
            subjectId: LIFECYCLE_IDS.submission,
            kind: "proof",
            expiresAtSlot: (slot + 300n).toString(),
          },
        ],
      };
    case "steps":
      return {
        ...records,
        submissions: records.submissions.map((entry) =>
          entry.submissionId === LIFECYCLE_IDS.submission
            ? { ...entry, status: "submitted" as const }
            : entry,
        ),
        retries: [
          ...records.retries,
          {
            retryId: LIFECYCLE_IDS.retry,
            submissionId: LIFECYCLE_IDS.submission,
            attempt: "1",
            nextEligibleSlot: (slot + 60n).toString(),
            reason: "confirmation_timeout",
          },
        ],
        deadlines: [
          ...records.deadlines,
          {
            deadlineId: LIFECYCLE_IDS.confirmationDeadline,
            subjectKind: "submission",
            subjectId: LIFECYCLE_IDS.submission,
            kind: "confirmation",
            expiresAtSlot: (slot + 600n).toString(),
          },
        ],
      };
    case "proof_token":
      return {
        ...records,
        confirmations: [
          ...records.confirmations,
          {
            confirmationId: LIFECYCLE_IDS.confirmation,
            submissionId: LIFECYCLE_IDS.submission,
            txHash: hex32("c6"),
            chainPointId: LIFECYCLE_IDS.chainPoint,
            depth: "1",
            status: "observed",
          },
        ],
      };
    case "removal_slashing":
      return {
        ...records,
        decisions: records.decisions.map((entry) =>
          entry.blockHash === LIFECYCLE_IDS.blockHash
            ? { ...entry, decision: "fault_proven" as const }
            : entry,
        ),
        confirmations: records.confirmations.map((entry) =>
          entry.confirmationId === LIFECYCLE_IDS.confirmation
            ? {
                ...entry,
                status: "confirmed" as const,
                depth: LIFECYCLE_POINT.depth,
              }
            : entry,
        ),
        correctionResults: [
          ...records.correctionResults,
          {
            correctionId: LIFECYCLE_IDS.correction,
            faultId: LIFECYCLE_IDS.fault,
            confirmationId: LIFECYCLE_IDS.confirmation,
            outcome: "removed_slashed_and_rewarded",
            finalStateRoot: hex32("c7"),
            slashLovelace: LIFECYCLE_SLASH_LOVELACE,
            rewardLovelace: LIFECYCLE_REWARD_LOVELACE,
          },
        ],
      };
    case "terminal_verification":
      return {
        ...records,
        decisions: records.decisions.map((entry) =>
          entry.blockHash === LIFECYCLE_IDS.blockHash
            ? { ...entry, decision: "removed_or_resolved" as const }
            : entry,
        ),
        retries: [],
        deadlines: [],
      };
  }
};

/**
 * The idempotence key of each transition: the exact durable record whose
 * presence proves the transition already landed. A restart consults only the
 * durable snapshot, never any in-process memory.
 */
const lifecycleTransitionAppliedV1 = (
  store: WatcherDurableStoreV1,
  transition: W44LifecycleTransitionV1,
): boolean => {
  switch (transition) {
    case "detect":
      return store.chainPoints.some(
        ({ chainPointId }) => chainPointId === LIFECYCLE_IDS.chainPoint,
      );
    case "persist_evidence":
      return store.faults.some(
        ({ faultId }) => faultId === LIFECYCLE_IDS.fault,
      );
    case "init":
      return store.submissions.some(
        ({ submissionId }) => submissionId === LIFECYCLE_IDS.submission,
      );
    case "steps":
      return store.submissions.some(
        ({ submissionId, status }) =>
          submissionId === LIFECYCLE_IDS.submission && status === "submitted",
      );
    case "proof_token":
      return store.confirmations.some(
        ({ confirmationId }) => confirmationId === LIFECYCLE_IDS.confirmation,
      );
    case "removal_slashing":
      return store.correctionResults.some(
        ({ correctionId }) => correctionId === LIFECYCLE_IDS.correction,
      );
    case "terminal_verification":
      return store.decisions.some(
        ({ blockHash, decision }) =>
          blockHash === LIFECYCLE_IDS.blockHash &&
          decision === "removed_or_resolved",
      );
  }
};

type LifecycleStepOutcome = Readonly<{
  transition: W44LifecycleTransitionV1;
  outcome: "applied" | "already_applied";
}>;

/**
 * The complete recovery procedure. It is the *only* way this file ever
 * advances or repairs durable state: read the atomic snapshot, skip every
 * transition whose durable key is already present, and compare-and-swap the
 * next one. Restarting after a crash is a re-invocation of this function.
 */
const driveWatcherLifecycleV1 = async (
  backend: CrashInjectingAtomicBackend,
  marker: ReturnType<typeof makeDeploymentMarkerV1>,
  publicObservation: WatcherNormalizedL1BlockV1,
): Promise<readonly LifecycleStepOutcome[]> => {
  const outcomes: LifecycleStepOutcome[] = [];
  for (const transition of W44_LIFECYCLE_TRANSITIONS_V1) {
    const snapshot = await readWatcherDurableAtomicSnapshotV1(backend);
    if (snapshot === null) {
      throw new Error("watcher durable snapshot missing on restart");
    }
    const store = decodeWatcherDurableStoreV1(snapshot.bytes);
    if (lifecycleTransitionAppliedV1(store, transition)) {
      outcomes.push({ transition, outcome: "already_applied" });
      continue;
    }
    const next = makeWatcherDurableStoreV1({
      deploymentMarker: marker,
      revision: (BigInt(store.revision) + 1n).toString(),
      records: applyLifecycleTransitionV1(
        recordsOf(store),
        transition,
        publicObservation,
      ),
    });
    const commit = await compareAndSwapWatcherDurableAtomicSnapshotV1({
      backend,
      expectedSha256: snapshot.sha256,
      next: encodeWatcherDurableStoreV1(next),
    });
    if (!commit.committed) {
      throw new Error(`watcher lifecycle conflict at ${transition}`);
    }
    outcomes.push({ transition, outcome: "applied" });
  }
  return outcomes;
};

/* ------------------------------------------------------------------------ */
/* Invariants                                                                */
/* ------------------------------------------------------------------------ */

type WatcherEvidenceSetV1 = Readonly<{
  faultIds: readonly string[];
  proofInputIds: readonly string[];
  reconstructedBlockHashes: readonly string[];
  observationIds: readonly string[];
  correctionIds: readonly string[];
}>;

const evidenceOf = (store: WatcherDurableStoreV1): WatcherEvidenceSetV1 => ({
  faultIds: store.faults.map(({ faultId }) => faultId).sort(),
  proofInputIds: store.daProofInputs.map(({ inputId }) => inputId).sort(),
  reconstructedBlockHashes: store.reconstructedStates
    .map(({ blockHash }) => blockHash)
    .sort(),
  observationIds: store.l1Observations
    .map(({ observationId }) => observationId)
    .sort(),
  correctionIds: store.correctionResults
    .map(({ correctionId }) => correctionId)
    .sort(),
});

/**
 * The evidence a rewind or recovery is *allowed* to drop is exactly the set it
 * reports as removed. Subtracting the reported set from the baseline turns
 * "no lost evidence" into an exact accounting rather than a weaker subset
 * claim: anything the engine drops silently is still counted as lost.
 */
const retainedBaselineV1 = (
  baseline: WatcherEvidenceSetV1,
  removed: WatcherRollbackRemovedRecordsV1,
): WatcherEvidenceSetV1 => {
  const without = (
    values: readonly string[],
    dropped: readonly string[],
  ): readonly string[] => {
    const droppedSet = new Set(dropped);
    return values.filter((value) => !droppedSet.has(value));
  };
  return {
    faultIds: without(baseline.faultIds, removed.faultIds),
    proofInputIds: without(baseline.proofInputIds, removed.daProofInputIds),
    reconstructedBlockHashes: without(
      baseline.reconstructedBlockHashes,
      removed.reconstructedBlockHashes,
    ),
    observationIds: without(baseline.observationIds, removed.l1ObservationIds),
    correctionIds: without(baseline.correctionIds, removed.correctionResultIds),
  };
};

const countDuplicates = (values: readonly string[]): number =>
  values.length - new Set(values).size;

type WatcherWorkflowMeasurementV1 = Readonly<{
  doubleSubmits: number;
  duplicateRewards: number;
  lostEvidence: number;
  falseVerifiedStates: number;
  unrecoverableWorkflows: number;
  publicDataViolations: number;
  sourceConsistencyViolations: number;
  maturityViolations: number;
  disabledFamilyFaults: number;
  ready: boolean;
}>;

/** Values that must never reach a durable public-data store. */
const PRIVATE_MATERIAL_MARKERS: readonly string[] = [
  Buffer.from(rollbackAuthorityKey).toString("hex"),
  Buffer.from("MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY", "utf8").toString("hex"),
  Buffer.from("MIDGARD_WATCHER_PROVER_KEY", "utf8").toString("hex"),
  Buffer.from("/var/lib/midgard-watcher", "utf8").toString("hex"),
];

const measureWatcherWorkflowV1 = (
  store: WatcherDurableStoreV1,
  baseline: WatcherEvidenceSetV1,
): WatcherWorkflowMeasurementV1 => {
  const current = evidenceOf(store);
  const missing = (
    before: readonly string[],
    after: readonly string[],
  ): number => {
    const present = new Set(after);
    return before.filter((value) => !present.has(value)).length;
  };

  const submissionsByFault = new Map<string, number>();
  for (const { faultId } of store.submissions) {
    submissionsByFault.set(faultId, (submissionsByFault.get(faultId) ?? 0) + 1);
  }
  const rewardsByFault = new Map<string, number>();
  for (const { faultId } of store.correctionResults) {
    rewardsByFault.set(faultId, (rewardsByFault.get(faultId) ?? 0) + 1);
  }

  const doubleSubmits =
    countDuplicates(store.submissions.map(({ submissionId }) => submissionId)) +
    [...submissionsByFault.values()].filter((count) => count > 1).length +
    countDuplicates(store.confirmations.map(({ txHash }) => txHash));

  const duplicateRewards =
    countDuplicates(
      store.correctionResults.map(({ correctionId }) => correctionId),
    ) + [...rewardsByFault.values()].filter((count) => count > 1).length;

  const faultBlockHashes = new Set(
    store.faults.map(({ blockHash }) => blockHash),
  );
  const correctedFaultIds = new Set(
    store.correctionResults.map(({ faultId }) => faultId),
  );
  const falseVerifiedStates =
    store.decisions.filter(
      ({ blockHash, decision }) =>
        decision === "verified" && faultBlockHashes.has(blockHash),
    ).length +
    store.decisions.filter(
      ({ blockHash, decision }) =>
        decision === "removed_or_resolved" &&
        store.faults
          .filter((fault) => fault.blockHash === blockHash)
          .some((fault) => !correctedFaultIds.has(fault.faultId)),
    ).length;

  // A workflow is unrecoverable when it is neither terminal nor has a defined
  // next lifecycle transition the deterministic driver can take.
  const nextTransition = W44_LIFECYCLE_TRANSITIONS_V1.find(
    (transition) => !lifecycleTransitionAppliedV1(store, transition),
  );
  const terminal = lifecycleTransitionAppliedV1(store, "terminal_verification");
  const unrecoverableWorkflows =
    !terminal && nextTransition === undefined ? store.faults.length : 0;

  const encoded = Buffer.from(encodeWatcherDurableStoreV1(store)).toString(
    "hex",
  );
  const publicDataViolations = PRIVATE_MATERIAL_MARKERS.filter((marker) =>
    encoded.includes(marker),
  ).length;

  const sourceConsistencyViolations =
    store.l1Observations.filter(
      ({ providerId }) => !CONFIGURED_PROVIDER_IDS.has(providerId),
    ).length +
    store.chainPoints.filter(
      ({ providerId }) => !CONFIGURED_PROVIDER_IDS.has(providerId),
    ).length;

  const evidenceSlots = store.chainPoints.map(({ slot }) => BigInt(slot));
  const earliestEvidenceSlot =
    evidenceSlots.length === 0
      ? null
      : evidenceSlots.reduce((low, slot) => (slot < low ? slot : low));
  const maturityViolations =
    earliestEvidenceSlot === null
      ? 0
      : store.deadlines.filter(({ expiresAtSlot }) => {
          const expiry = BigInt(expiresAtSlot);
          return (
            expiry <= earliestEvidenceSlot ||
            expiry > earliestEvidenceSlot + MATURITY_BUDGET_SLOTS
          );
        }).length;

  const disabledFamilyFaults = store.faults.filter(
    ({ familyId }) => !ENABLED_FAMILY_IDS.has(familyId),
  ).length;

  const ready =
    store.faults.length > 0 &&
    store.faults.every(({ faultId }) => correctedFaultIds.has(faultId)) &&
    store.decisions.every(({ decision }) => decision !== "fault_detected") &&
    store.deadlines.length === 0 &&
    terminal;

  return {
    doubleSubmits,
    duplicateRewards,
    lostEvidence:
      missing(baseline.faultIds, current.faultIds) +
      missing(baseline.proofInputIds, current.proofInputIds) +
      missing(
        baseline.reconstructedBlockHashes,
        current.reconstructedBlockHashes,
      ) +
      missing(baseline.observationIds, current.observationIds) +
      missing(baseline.correctionIds, current.correctionIds),
    falseVerifiedStates,
    unrecoverableWorkflows,
    publicDataViolations,
    sourceConsistencyViolations,
    maturityViolations,
    disabledFamilyFaults,
    ready,
  };
};

const ZERO_DEFECTS = Object.freeze({
  doubleSubmits: 0,
  duplicateRewards: 0,
  lostEvidence: 0,
  falseVerifiedStates: 0,
  unrecoverableWorkflows: 0,
  publicDataViolations: 0,
  sourceConsistencyViolations: 0,
  maturityViolations: 0,
  disabledFamilyFaults: 0,
});

/* ------------------------------------------------------------------------ */
/* Rollback fixtures                                                         */
/* ------------------------------------------------------------------------ */

type Graph = Readonly<{ records: WatcherDurableRecordsV1 }>;

const graph = (idByte: string, point: Point): Graph => {
  const ids = {
    observation: hex32(`${idByte[0]!}1`),
    chainPoint: hex32(`${idByte[0]!}2`),
    outRef: `${hex32(`${idByte[0]!}3`)}#0`,
    input: hex32(`${idByte[0]!}4`),
    blockHash: hex32(`${idByte[0]!}0`),
    fault: hex32(`${idByte[0]!}5`),
    submission: hex32(`${idByte[0]!}6`),
    confirmation: hex32(`${idByte[0]!}7`),
    retry: hex32(`${idByte[0]!}8`),
    deadline: hex32(`${idByte[0]!}9`),
    correction: hex32(`${idByte[0]!}a`),
  };
  return {
    records: {
      l1Observations: [
        {
          observationId: ids.observation,
          providerId: "provider-a",
          chainPointId: ids.chainPoint,
          payload: payload("8100"),
        },
      ],
      chainPoints: [
        {
          chainPointId: ids.chainPoint,
          providerId: "provider-a",
          blockHash: point.blockHash,
          slot: point.slot,
          blockNo: point.blockNo,
          depth: point.depth,
        },
      ],
      protocolUtxos: [
        {
          outRef: ids.outRef,
          role: "state_queue",
          chainPointId: ids.chainPoint,
          output: payload("d87980"),
        },
      ],
      spentProtocolUtxos: [],
      daProofInputs: [
        {
          inputId: ids.input,
          kind: "da_payload",
          payload: payload("4401020304"),
        },
      ],
      reconstructedStates: [
        {
          blockHash: ids.blockHash,
          chainPointId: ids.chainPoint,
          priorStateRoot: hex32(`${idByte[0]!}b`),
          postStateRoot: hex32(`${idByte[0]!}c`),
          inputIds: [ids.input],
          state: payload("82190100190101"),
        },
      ],
      decisions: [
        {
          blockHash: ids.blockHash,
          decision: "fault_detected",
          reconstructionDigest: hex32(`${idByte[0]!}d`),
          evidenceDigest: hex32(`${idByte[0]!}e`),
        },
      ],
      faults: [
        {
          faultId: ids.fault,
          blockHash: ids.blockHash,
          familyId: LIFECYCLE_FAMILY_ID,
          evidence: payload("a10001"),
        },
      ],
      submissions: [
        {
          submissionId: ids.submission,
          faultId: ids.fault,
          txBodyHash: hex32(`${idByte[0]!}f`),
          status: "submitted",
        },
      ],
      confirmations: [
        {
          confirmationId: ids.confirmation,
          submissionId: ids.submission,
          txHash: hex32(`${idByte[0]!}${idByte[1]!}`),
          chainPointId: ids.chainPoint,
          depth: point.depth,
          status: "confirmed",
        },
      ],
      retries: [
        {
          retryId: ids.retry,
          submissionId: ids.submission,
          attempt: "1",
          nextEligibleSlot: (BigInt(point.slot) + 1n).toString(),
          reason: "rollback",
        },
      ],
      deadlines: [
        {
          deadlineId: ids.deadline,
          subjectKind: "submission",
          subjectId: ids.submission,
          kind: "rollback",
          expiresAtSlot: (BigInt(point.slot) + 10n).toString(),
        },
      ],
      correctionResults: [
        {
          correctionId: ids.correction,
          faultId: ids.fault,
          confirmationId: ids.confirmation,
          outcome: "removed",
          finalStateRoot: hex32(`${idByte[1]!}${idByte[0]!}`),
          slashLovelace: "0",
          rewardLovelace: "0",
        },
      ],
    },
  };
};

const combine = (
  deploymentMarker: ReturnType<typeof makeDeploymentMarkerV1>,
  revision: string,
  graphs: readonly Graph[],
  persistedObservations: readonly WatcherNormalizedL1BlockV1[] = [],
): WatcherDurableStoreV1 => {
  const persistedChainPoints = [
    ...new Map(
      persistedObservations.map((value) => [
        value.chainPoint.chainPointId,
        {
          chainPointId: value.chainPoint.chainPointId,
          providerId: value.provider.providerId,
          blockHash: value.chainPoint.blockHash,
          slot: value.chainPoint.slot,
          blockNo: value.chainPoint.blockNo,
          depth: value.chainPoint.depth,
        },
      ]),
    ).values(),
  ];
  return makeWatcherDurableStoreV1({
    deploymentMarker,
    revision,
    records: {
      l1Observations: graphs
        .flatMap(({ records }) => records.l1Observations)
        .concat(
          persistedObservations.map((value) => ({
            observationId: value.observationDigest,
            providerId: value.provider.providerId,
            chainPointId: value.chainPoint.chainPointId,
            payload: makeWatcherDurablePayloadV1(
              encodeWatcherNormalizedL1BlockV1(value).toString("hex"),
            ),
          })),
        ),
      chainPoints: graphs
        .flatMap(({ records }) => records.chainPoints)
        .concat(persistedChainPoints),
      protocolUtxos: graphs.flatMap(({ records }) => records.protocolUtxos),
      spentProtocolUtxos: graphs.flatMap(
        ({ records }) => records.spentProtocolUtxos,
      ),
      daProofInputs: graphs.flatMap(({ records }) => records.daProofInputs),
      reconstructedStates: graphs.flatMap(
        ({ records }) => records.reconstructedStates,
      ),
      decisions: graphs.flatMap(({ records }) => records.decisions),
      faults: graphs.flatMap(({ records }) => records.faults),
      submissions: graphs.flatMap(({ records }) => records.submissions),
      confirmations: graphs.flatMap(({ records }) => records.confirmations),
      retries: graphs.flatMap(({ records }) => records.retries),
      deadlines: graphs.flatMap(({ records }) => records.deadlines),
      correctionResults: graphs.flatMap(
        ({ records }) => records.correctionResults,
      ),
    },
  });
};

const oldPoint: Point = Object.freeze({
  blockHash: hex32("aa"),
  slot: "1000",
  blockNo: "100",
  depth: "1",
});
const replacementPoint: Point = Object.freeze({
  blockHash: hex32("bb"),
  slot: "1001",
  blockNo: "101",
  depth: "2",
});

const recoveryAgreement = (point: Point) => {
  const observations = agreementObservations(point);
  return {
    observations,
    consistency: evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      observations,
      watcherTransportAttestations,
    ),
  };
};

const recoveryPoints = (
  branch: "old" | "replacement",
  common: Point,
  length: number,
  finalDepth: string,
): readonly Point[] => {
  const points: Point[] = [common];
  for (let index = 1; index <= length; index += 1) {
    const previous = points.at(-1)!;
    points.push({
      blockHash: sha256Canonical({ branch, index }),
      parentBlockHash: previous.blockHash,
      blockNo: (BigInt(common.blockNo) + BigInt(index)).toString(),
      slot: (BigInt(common.slot) + BigInt(index)).toString(),
      depth: index === length ? finalDepth : "0",
    });
  }
  return points;
};

/**
 * Builds a finalized-depth rollback: the previous canonical branch is
 * `rollbackDepth` blocks long and its tip is finalized at the configured
 * depth, so the contradicting replacement branch is a rollback strictly
 * deeper than `finalityDepth` and within the fixed Cardano k bound.
 */
const postFinalityRecoveryFixture = (rollbackDepth: number) => {
  const finalityPolicy = policy();
  const common: Point = {
    blockHash: hex32("01"),
    parentBlockHash: hex32("00"),
    blockNo: "1000",
    slot: "1000",
    depth: "0",
  };
  const previousBundles = recoveryPoints(
    "old",
    common,
    rollbackDepth,
    String(FINALITY_DEPTH),
  ).map(recoveryAgreement);
  const replacementBundles = recoveryPoints("replacement", common, 2, "0").map(
    recoveryAgreement,
  );
  const orphanedTip = previousBundles.at(-1)!;
  const replacementTip = replacementBundles.at(-1)!;
  const pendingTip = recoveryAgreement({
    ...orphanedTip.observations[0]!.chainPoint,
    depth: "2",
  });
  const pendingState = evaluateWatcherFinalityV1(
    finalityPolicy,
    null,
    pendingTip.consistency,
  ).state as WatcherFinalityStateV1;
  const finalizedState = evaluateWatcherFinalityV1(
    finalityPolicy,
    pendingState,
    orphanedTip.consistency,
  ).state as WatcherFinalityStateV1;
  expect(finalizedState.phase).toBe("finalized");
  const contradiction = evaluateWatcherFinalityV1(
    finalityPolicy,
    finalizedState,
    replacementTip.consistency,
  );
  expect(contradiction.action).toBe("quarantine_incident");
  const persistedObservations = [
    ...new Map(
      [...previousBundles, ...replacementBundles]
        .flatMap(({ observations }) => observations)
        .map((entry) => [entry.observationDigest, entry]),
    ).values(),
  ];
  const orphanedGraph = graph("10", orphanedTip.observations[0]!.chainPoint);
  const commonGraph = graph("40", common);
  const store = combine(
    finalityPolicy.deploymentMarker,
    "11",
    [orphanedGraph, commonGraph],
    persistedObservations,
  );
  const rollbackBootstrapState = makeWatcherRollbackBootstrapStateV1(
    finalityPolicy,
    store,
    finalizedState,
  );
  expect(rollbackBootstrapState).not.toBeNull();
  return {
    finalityPolicy,
    initialStore: store,
    finalizedState,
    contradictionConsistency: replacementTip.consistency,
    contradiction,
    previousPath: previousBundles.map(({ consistency }) => consistency),
    replacementPath: replacementBundles.map(({ consistency }) => consistency),
    rollbackDepth,
  };
};

/* ------------------------------------------------------------------------ */
/* Matrix                                                                    */
/* ------------------------------------------------------------------------ */

describe("W44 watcher crash and rollback matrix", () => {
  let marker: ReturnType<typeof makeDeploymentMarkerV1>;
  let lifecycleObservation: WatcherNormalizedL1BlockV1;

  beforeAll(async () => {
    const contexts = await Promise.all(
      ["a", "b"].map(async (suffix, index) => {
        const identity = testTlsIdentities[index]!;
        const server = createTlsServer(
          { cert: identity.cert, key: identity.key },
          (socket) => {
            socket.on("error", () => undefined);
          },
        );
        await listen(server, 0, "127.0.0.1");
        watcherTransportFixtureServers.push(server);
        const address = server.address();
        if (address === null || typeof address === "string") {
          throw new Error("missing W44 TLS fixture address");
        }
        const endpoint = `https://localhost:${address.port.toString()}/provider-${suffix}`;
        return {
          endpoint,
          established: await establishWatcherExternalProviderTransportV1({
            network: "Preprod",
            providerId: index === 0 ? "provider-a" : "provider-b",
            operatorIdentitySha256: index === 0 ? hex32("a1") : hex32("b2"),
            endpoint,
            caPem: identity.cert,
            expectedTlsPublicIdentitySha256: createHash("sha256")
              .update(new X509Certificate(identity.cert).raw)
              .digest("hex"),
            connectTimeoutMs: 5_000,
          }),
        };
      }),
    );
    externalProviderEndpoints = [contexts[0]!.endpoint, contexts[1]!.endpoint];
    externalProviderATransport = contexts[0]!.established;
    externalProviderBTransport = contexts[1]!.established;
    watcherTransportAttestations = Object.freeze([
      externalProviderATransport,
      externalProviderBTransport,
    ]);
    marker = makeDeploymentMarkerV1(hex32("11"));
    lifecycleObservation = observation("provider-a", LIFECYCLE_POINT);
  }, 30_000);

  afterAll(async () => {
    for (const context of watcherTransportAttestations) {
      closeWatcherL1TransportAttestationContextV1(context);
    }
    await Promise.all(
      watcherTransportFixtureServers.splice(0).map(closeServer),
    );
    watcherTransportFixtureServers = [];
    watcherTransportAttestations = [];
  });

  const freshBackend = async (): Promise<CrashInjectingAtomicBackend> => {
    const backend = new CrashInjectingAtomicBackend();
    await migrateWatcherDurableStoreV1({ backend, deploymentMarker: marker });
    backend.attempts = 0;
    backend.writes = 0;
    return backend;
  };

  /** The uncrashed control run every crash case must converge to. */
  const controlRun = async (): Promise<
    Readonly<{ digest: string; store: WatcherDurableStoreV1; writes: number }>
  > => {
    const backend = await freshBackend();
    const outcomes = await driveWatcherLifecycleV1(
      backend,
      marker,
      lifecycleObservation,
    );
    expect(outcomes.map(({ outcome }) => outcome)).toEqual(
      W44_LIFECYCLE_TRANSITIONS_V1.map(() => "applied"),
    );
    return {
      digest: backend.digest()!,
      store: backend.snapshotStore(),
      writes: backend.writes,
    };
  };

  const runCrashCase = async (
    transitionIndex: number,
    side: "before" | "after",
  ): Promise<void> => {
    const transition = W44_LIFECYCLE_TRANSITIONS_V1[transitionIndex]!;
    const control = await controlRun();
    expect(control.writes).toBe(W44_LIFECYCLE_TRANSITIONS_V1.length);

    const backend = await freshBackend();
    const attempt = transitionIndex + 1;
    if (side === "before") {
      backend.crashBeforeAttempt = attempt;
    } else {
      backend.crashAfterAttempt = attempt;
    }

    const preCrashBaseline = evidenceOf(backend.snapshotStore());
    await expect(
      driveWatcherLifecycleV1(backend, marker, lifecycleObservation),
    ).rejects.toMatchObject({ code: "persistence_failure" });

    // The durable snapshot is exactly the boundary the crash landed on: the
    // transition either did not happen at all, or happened exactly once.
    const expectedWrites = side === "before" ? attempt - 1 : attempt;
    expect(backend.writes).toBe(expectedWrites);
    const crashedStore = backend.snapshotStore();
    expect(lifecycleTransitionAppliedV1(crashedStore, transition)).toBe(
      side === "after",
    );
    expect(measureWatcherWorkflowV1(crashedStore, preCrashBaseline)).toEqual({
      ...ZERO_DEFECTS,
      // Readiness is never true from a partially advanced crash state; it is
      // only true once the terminal transition itself has landed.
      ready: transition === "terminal_verification" && side === "after",
    });

    const crashedEvidence = evidenceOf(crashedStore);

    // Adversarial control: a torn or tampered snapshot at the same boundary is
    // never silently accepted, so "recoverable" cannot be reached by trusting
    // arbitrary bytes left behind by a crash.
    const tampered = Uint8Array.from(backend.bytes!);
    tampered[tampered.length - 1] = (tampered.at(-1)! + 1) % 256;
    expect(() => decodeWatcherDurableStoreV1(tampered)).toThrowError();
    expect(() =>
      decodeWatcherDurableStoreV1(tampered.slice(0, tampered.length - 1)),
    ).toThrowError();

    // Restart: the same deterministic driver, no manual surgery.
    const restart = await driveWatcherLifecycleV1(
      backend,
      marker,
      lifecycleObservation,
    );
    const alreadyApplied = restart.filter(
      ({ outcome }) => outcome === "already_applied",
    ).length;
    expect(alreadyApplied).toBe(expectedWrites);
    expect(restart).toHaveLength(W44_LIFECYCLE_TRANSITIONS_V1.length);

    // Exactly seven durable writes across crash and restart: the crashed
    // transition is never applied twice.
    expect(backend.writes).toBe(W44_LIFECYCLE_TRANSITIONS_V1.length);
    expect(backend.digest()).toBe(control.digest);

    const recovered = backend.snapshotStore();
    expect(recovered).toEqual(control.store);
    expect(measureWatcherWorkflowV1(recovered, crashedEvidence)).toEqual({
      ...ZERO_DEFECTS,
      ready: true,
    });
    expect(recovered.correctionResults).toHaveLength(1);
    expect(recovered.correctionResults[0]).toMatchObject({
      outcome: "removed_slashed_and_rewarded",
      rewardLovelace: LIFECYCLE_REWARD_LOVELACE,
      slashLovelace: LIFECYCLE_SLASH_LOVELACE,
    });
    expect(recovered.submissions).toHaveLength(1);

    // Public-data condition: the only observation the recovered store carries
    // is the byte-exact canonical encoding of the authenticated public L1
    // observation, so recovery never depends on private operator state.
    expect(recovered.l1Observations).toHaveLength(1);
    expect(recovered.l1Observations[0]!.payload.cborHex).toBe(
      encodeWatcherNormalizedL1BlockV1(lifecycleObservation).toString("hex"),
    );

    // A second restart is a total no-op: recovery converges, it does not
    // oscillate.
    const idempotent = await driveWatcherLifecycleV1(
      backend,
      marker,
      lifecycleObservation,
    );
    expect(
      idempotent.every(({ outcome }) => outcome === "already_applied"),
    ).toBe(true);
    expect(backend.writes).toBe(W44_LIFECYCLE_TRANSITIONS_V1.length);
    expect(backend.digest()).toBe(control.digest);
  };

  it("recovers deterministically from a crash before the detect journal boundary", async () => {
    await runCrashCase(0, "before");
  });

  it("recovers deterministically from a crash after the detect journal boundary", async () => {
    await runCrashCase(0, "after");
  });

  it("recovers deterministically from a crash before the persist-evidence journal boundary", async () => {
    await runCrashCase(1, "before");
  });

  it("recovers deterministically from a crash after the persist-evidence journal boundary", async () => {
    await runCrashCase(1, "after");
  });

  it("recovers deterministically from a crash before the proof-init journal boundary", async () => {
    await runCrashCase(2, "before");
  });

  it("recovers deterministically from a crash after the proof-init journal boundary", async () => {
    await runCrashCase(2, "after");
  });

  it("recovers deterministically from a crash before the submit journal boundary", async () => {
    await runCrashCase(3, "before");
  });

  it("recovers deterministically from a crash after the submit journal boundary", async () => {
    await runCrashCase(3, "after");
  });

  it("recovers deterministically from a crash before the proof-token confirm boundary", async () => {
    await runCrashCase(4, "before");
  });

  it("recovers deterministically from a crash after the proof-token confirm boundary", async () => {
    await runCrashCase(4, "after");
  });

  it("recovers deterministically from a crash before the removal/slashing confirm boundary", async () => {
    await runCrashCase(5, "before");
  });

  it("recovers deterministically from a crash after the removal/slashing confirm boundary", async () => {
    await runCrashCase(5, "after");
  });

  it("recovers deterministically from a crash before the terminal-verification journal boundary", async () => {
    await runCrashCase(6, "before");
  });

  it("recovers deterministically from a crash after the terminal-verification journal boundary", async () => {
    await runCrashCase(6, "after");
  });

  it("rewinds and replays an ordinary pre-finality L1 rollback without losing evidence", async () => {
    const finalityPolicy = policy();
    const prior = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      agreement(oldPoint),
    ).state as WatcherFinalityStateV1;
    const consistency = agreement(replacementPoint);
    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      prior,
      consistency,
    );
    expect(finalityResult.action).toBe("rewind_pending");
    expect(BigInt(replacementPoint.depth)).toBeLessThan(BigInt(FINALITY_DEPTH));

    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      agreementObservations(replacementPoint),
    );
    const backend = new CrashInjectingAtomicBackend();
    const initialized = await initializeWatcherRollbackDurableAuthorityV1({
      backend,
      policy: finalityPolicy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: null,
      bootstrapStore: store,
      bootstrapFinalityState: prior,
    });
    const baseline = evidenceOf(store);

    const applied = await evaluateAndPersistWatcherRollbackV1({
      authority: initialized.authority,
      previousFinalityState: prior,
      consistency,
      finalityResult,
      transportAttestations: watcherTransportAttestations,
    });
    expect(applied).toMatchObject({
      persistence: "committed",
      result: { action: "apply_rewind" },
    });
    if (applied.persistence !== "committed") {
      throw new Error("expected a committed ordinary rewind");
    }
    const rewound = applied.result.nextStore!;
    const removed = applied.result.removedRecords;
    // The rewind sweeps exactly the orphaned lineage and reports it.
    expect(removed.faultIds.length).toBeGreaterThan(0);
    const measured = measureWatcherWorkflowV1(
      rewound,
      retainedBaselineV1(baseline, removed),
    );
    expect(measured).toMatchObject(ZERO_DEFECTS);
    expect(measured.ready).toBe(false);
    // The rewind must not fabricate a verified decision for a rewound block.
    expect(
      rewound.decisions.filter(({ decision }) => decision === "verified"),
    ).toHaveLength(0);

    const committedDigest = backend.digest();
    const reloaded = await loadWatcherRollbackDurableAuthorityV1({
      backend,
      policy: finalityPolicy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: applied.trustedHead,
    });
    const replayed = await evaluateAndPersistWatcherRollbackV1({
      authority: reloaded,
      previousFinalityState: prior,
      consistency,
      finalityResult,
      transportAttestations: watcherTransportAttestations,
    });
    expect(replayed).toMatchObject({
      persistence: "unchanged",
      result: { action: "duplicate_rewind" },
    });
    expect(backend.digest()).toBe(committedDigest);
    expect(backend.writes).toBe(2);
  }, 60_000);

  it("automatically recovers a rollback deeper than the finality depth within the k bound", async () => {
    const fixture = postFinalityRecoveryFixture(FINALITY_DEPTH + 3);
    expect(fixture.rollbackDepth).toBeGreaterThan(FINALITY_DEPTH);
    expect(BigInt(fixture.rollbackDepth)).toBeLessThanOrEqual(
      WATCHER_ROLLBACK_V1_BOUNDS.postFinalityRecoveryDepth,
    );

    const backend = new CrashInjectingAtomicBackend();
    const initialized = await initializeWatcherRollbackDurableAuthorityV1({
      backend,
      policy: fixture.finalityPolicy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: null,
      bootstrapStore: fixture.initialStore,
      bootstrapFinalityState: fixture.finalizedState,
    });
    const baseline = evidenceOf(fixture.initialStore);

    const incident = await evaluateAndPersistWatcherRollbackV1({
      authority: initialized.authority,
      previousFinalityState: fixture.finalizedState,
      consistency: fixture.contradictionConsistency,
      finalityResult: fixture.contradiction,
      transportAttestations: watcherTransportAttestations,
    });
    expect(incident).toMatchObject({
      persistence: "committed",
      result: {
        action: "quarantine_incident",
        protocolDecision: "quarantined",
      },
    });
    if (incident.persistence !== "committed") {
      throw new Error("expected a committed post-finality incident");
    }
    // Quarantine must never read as ready and must never lose evidence.
    const quarantined = incident.result.nextStore ?? fixture.initialStore;
    const quarantineMeasurement = measureWatcherWorkflowV1(
      quarantined,
      retainedBaselineV1(baseline, incident.result.removedRecords),
    );
    expect(quarantineMeasurement).toMatchObject(ZERO_DEFECTS);
    expect(quarantineMeasurement.ready).toBe(false);

    const recovered = await evaluateAndPersistWatcherPostFinalityRecoveryV1({
      authority: incident.authority,
      previousCanonicalPath: fixture.previousPath,
      replacementCanonicalPath: fixture.replacementPath,
      transportAttestations: watcherTransportAttestations,
    });
    expect(recovered).toMatchObject({
      persistence: "committed",
      result: {
        action: "rewind_and_replay",
        protocolDecision: "resume_replay",
      },
    });
    if (recovered.persistence !== "committed") {
      throw new Error("expected a committed post-finality recovery");
    }
    expect(
      watcherRollbackDurableAuthorityStatusV1(recovered.authority),
    ).toMatchObject({ revision: "2" });
    const recoveredStore = recovered.result.nextStore!;
    expect(
      recoveredStore.decisions.filter(
        ({ decision }) => decision === "verified",
      ),
    ).toHaveLength(0);
    const recoveredMeasurement = measureWatcherWorkflowV1(
      recoveredStore,
      retainedBaselineV1(
        retainedBaselineV1(baseline, incident.result.removedRecords),
        recovered.result.removedRecords,
      ),
    );
    expect(recoveredMeasurement).toMatchObject(ZERO_DEFECTS);
    expect(recoveredMeasurement.ready).toBe(false);

    const recoveredDigest = backend.digest();
    const replayAuthority = await loadWatcherRollbackDurableAuthorityV1({
      backend,
      policy: fixture.finalityPolicy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: recovered.trustedHead,
    });
    const replayed = await evaluateAndPersistWatcherPostFinalityRecoveryV1({
      authority: replayAuthority,
      previousCanonicalPath: fixture.previousPath,
      replacementCanonicalPath: fixture.replacementPath,
      transportAttestations: watcherTransportAttestations,
    });
    expect(replayed.persistence).toBe("unchanged");
    expect(backend.digest()).toBe(recoveredDigest);
    expect(backend.writes).toBe(3);
  }, 120_000);

  it("fails closed on a configured-source inconsistency without mutating durable state", async () => {
    const finalityPolicy = policy();
    const prior = evaluateWatcherFinalityV1(
      finalityPolicy,
      null,
      agreement(oldPoint),
    ).state as WatcherFinalityStateV1;
    const store = combine(
      finalityPolicy.deploymentMarker,
      "0",
      [graph("10", oldPoint), graph("20", replacementPoint)],
      agreementObservations(replacementPoint),
    );

    // The two configured providers report different chain points at the same
    // height: the configured source is inconsistent.
    const disagreement = evaluateWatcherMultiProviderConsistencyV1(
      externalSource(),
      [
        observation("provider-a", replacementPoint),
        observation("provider-b", {
          ...replacementPoint,
          blockHash: hex32("ee"),
        }),
      ],
      watcherTransportAttestations,
    );
    expect(disagreement).toMatchObject({
      status: "quarantined",
      protocolDecision: "quarantined",
    });

    const finalityResult = evaluateWatcherFinalityV1(
      finalityPolicy,
      prior,
      disagreement,
    );
    expect(finalityResult.action).not.toBe("rewind_pending");

    const bootstrapState = makeWatcherRollbackBootstrapStateV1(
      finalityPolicy,
      store,
      prior,
    );
    expect(bootstrapState).not.toBeNull();
    const rejected = evaluateWatcherRollbackV1(
      finalityPolicy,
      store,
      prior,
      disagreement,
      finalityResult,
      bootstrapState,
      bootstrapState,
    );
    expect(rejected.action).toBe("reject");
    expect(rejected.nextStore).toBeNull();

    const backend = new CrashInjectingAtomicBackend();
    const initialized = await initializeWatcherRollbackDurableAuthorityV1({
      backend,
      policy: finalityPolicy,
      authenticationKey: rollbackAuthorityKey,
      trustedHead: null,
      bootstrapStore: store,
      bootstrapFinalityState: prior,
    });
    const initialDigest = backend.digest();
    const baseline = evidenceOf(store);

    const persisted = await evaluateAndPersistWatcherRollbackV1({
      authority: initialized.authority,
      previousFinalityState: prior,
      consistency: disagreement,
      finalityResult,
      transportAttestations: watcherTransportAttestations,
    });
    expect(persisted).toMatchObject({
      persistence: "unchanged",
      result: { action: "reject" },
    });
    expect(backend.digest()).toBe(initialDigest);
    expect(backend.writes).toBe(1);

    const measured = measureWatcherWorkflowV1(store, baseline);
    expect(measured).toMatchObject(ZERO_DEFECTS);
    expect(measured.ready).toBe(false);
  }, 60_000);
});
