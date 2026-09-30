import { type Server } from "node:net";

import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

import { makeWatcherL1PublicBytes } from "../../src/l1/l1-adapter.js";
import {
  decodeWatcherDurableStore,
  makeWatcherDurablePayload,
  type WatcherDurableAtomicBackend,
  type WatcherDurableRecords,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
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

export const hex32 = (byte: string): string => byte.repeat(32);

export const testTlsIdentities = [
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

export const payload = (
  cborHex = "80",
): ReturnType<typeof makeWatcherDurablePayload> =>
  makeWatcherDurablePayload(cborHex);

export const rollbackAuthorityKey = Uint8Array.from(
  { length: 32 },
  (_, index) => index + 1,
);

/**
 * A durable backend that can be crashed at an exact compare-and-swap boundary.
 *
 * `crashBeforeAttempt` throws before the bytes are replaced (the transition is
 * lost). `crashAfterAttempt` throws after the bytes are durably replaced (the
 * caller cannot tell whether the transition landed). Both are the ambiguous
 * windows a real process crash produces around a journal boundary.
 */
export class CrashInjectingAtomicBackend
  implements WatcherDurableAtomicBackend
{
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

  snapshotStore(): WatcherDurableStore {
    if (this.bytes === null) {
      throw new Error("backend has no durable snapshot");
    }
    return decodeWatcherDurableStore(this.bytes);
  }
}

export const listen = (
  server: Server,
  port: number,
  host: string,
): Promise<void> =>
  new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, host, () => {
      server.off("error", reject);
      resolve();
    });
  });

export const closeServer = (server: Server): Promise<void> =>
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

export const deploymentIdentity = (
  manifestByte = "11",
  releaseByte = "22",
) => ({
  manifestId: hex32(manifestByte),
  network: "Preprod" as const,
  trustRootId: hex32("33"),
  fundingProfileBundleDigest: "ab".repeat(32),
  blueprintHash: hex32(releaseByte),
  ruleBundleCommitment: hex32("44"),
  programCommitments: { validation: hex32("55") },
  durableMarker: makeDeploymentMarker(hex32(manifestByte)),
});

export type Point = Readonly<{
  blockHash: string;
  parentBlockHash?: string | null;
  slot: string;
  blockNo: string;
  depth: string;
  bodyHex?: string;
}>;

export const transaction = (seedHex: string) => {
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
    fullTransaction: makeWatcherL1PublicBytes(
      fullTransaction.to_canonical_cbor_hex(),
    ),
    body: makeWatcherL1PublicBytes(bodyBytes),
    witnessSet: makeWatcherL1PublicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: [],
    scripts: [],
    datums: [],
    redeemers: [],
  };
};

export const recordsOf = (
  store: WatcherDurableStore,
): WatcherDurableRecords => ({
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

/* ------------------------------------------------------------------------ */
/* Invariants                                                                */
/* ------------------------------------------------------------------------ */

export type WatcherEvidenceSet = Readonly<{
  faultIds: readonly string[];
  proofInputIds: readonly string[];
  reconstructedBlockHashes: readonly string[];
  observationIds: readonly string[];
  correctionIds: readonly string[];
}>;
