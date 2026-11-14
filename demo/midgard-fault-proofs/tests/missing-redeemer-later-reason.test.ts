/**
 * The installed missing-redeemer replay over a forced rejection whose recorded
 * reason names no purpose coordinate: the subject's reference-input observer
 * has a Plutus source and no redeemer, so a missing redeemer at that observer
 * is detected under any reason the purpose phase does not admit before it,
 * and never under one it admits. A rejection whose purpose fields the replay
 * cannot open yields no detection and does not stop the replay; an output's
 * reference script is not on the purpose walk. An unresolved replay
 * prerequisite behind such a rejection still stops the replay.
 */
import {
  materializeMidgardNativeTxFromCanonical,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  encodeMidgardValue,
} from "@al-ft/midgard-core/codec";
import {
  encodeCbor,
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { ledgerEntriesRoot, singleLedgerAt } from "../src/ledger-at.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { CanonicalReplayPrerequisiteError } from "../src/workflow/replay-prerequisite.js";
import { authenticatedHeaderObservation } from "./helpers/canonical-block-evidence-fixture.js";
import {
  protectedScriptAddress,
  redeemerItem,
} from "./support/missing-redeemer-emulator.purposes.js";
import {
  buildMissingRedeemerSubject,
  FORCED_ORDER_KEY,
  honestForcedObserverShape,
  type MissingRedeemerSubjectShape,
  plutusScript,
  redeemerMissingVerdict,
} from "./support/missing-redeemer-emulator.subject.js";
import {
  buildDecodingBlockFixture,
  decodingSubjectTransaction,
} from "./support/native-script-decoding-emulator.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/missing-redeemer-later-reason",
  grade: "security",
} as const;

const shape = honestForcedObserverShape(5);

const rejected = (reason: SDK.RejectionReason): SDK.OperatorVerdict => ({
  ForcedTxInvalid: { reason },
});
const plutusExecutionFailed = rejected({
  PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n },
});
const unusedRedeemer = rejected({ UnusedRedeemer: { redeemer_index: 0n } });

/** Installed replay of a block committing `nativeTx` as the forced leaf,
 * with any further events `layout` commits beside it. */
const replayOf = async (
  subjectShape: MissingRedeemerSubjectShape,
  nativeTx: MidgardNativeTxFull,
  verdict: SDK.OperatorVerdict,
  layout: Pick<
    Parameters<typeof buildDecodingBlockFixture>[0],
    "additionalTransactions" | "orderEvents"
  > = {},
) => {
  const entries = [
    ...(subjectShape.spend ?? []),
    ...(subjectShape.reference ?? []),
  ];
  const root = await ledgerEntriesRoot(entries);
  const block = await buildDecodingBlockFixture({
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    priorLedgerRoot: root,
    prevUtxosRoot: root,
    subject: {
      kind: "forced",
      nativeTx,
      orderKey: FORCED_ORDER_KEY,
      verdict,
    },
    ...layout,
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({
      header: block.header,
      headerHash: await Effect.runPromise(SDK.hashBlockHeader(block.header)),
    }),
    payloadEnvelopeCbor: block.reconstruction.payloadEnvelopeCbor,
    daProvenance,
  });
  const decision = await MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY.replay(
    evidence,
    {
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        ledgerAt: singleLedgerAt(evidence.header.prevHeaderHash, entries),
        currentEvidence: evidence,
      }),
    },
  );
  return decision.detections.map((detection) => detection.detectionId);
};

const replayUnder = (
  verdict: SDK.OperatorVerdict,
  subjectShape: MissingRedeemerSubjectShape = shape,
) =>
  replayOf(
    subjectShape,
    buildMissingRedeemerSubject(subjectShape).nativeTx,
    verdict,
  );

/** The subject with some of its canonical field preimages replaced. */
const withFields = (
  patch: Readonly<{
    outputsPreimageCbor?: Buffer;
    scriptTxWitsPreimageCbor?: Buffer;
    redeemerTxWitsPreimageCbor?: Buffer;
  }>,
): MidgardNativeTxFull => {
  const { nativeTx } = buildMissingRedeemerSubject(shape);
  const { outputsPreimageCbor, ...witnessSet } = patch;
  return materializeMidgardNativeTxFromCanonical({
    version: nativeTx.version,
    validity: nativeTx.validity,
    body: {
      ...nativeTx.body,
      ...(outputsPreimageCbor === undefined ? {} : { outputsPreimageCbor }),
    },
    witnessSet: { ...nativeTx.witnessSet, ...witnessSet },
  });
};

const receiver = plutusScript(9);
const value = encodeMidgardValue({ lovelace: 2_000_000n, assets: new Map() });

/** A redeemer item followed by a trailing byte. */
const nonCanonicalRedeemer = () =>
  withFields({
    redeemerTxWitsPreimageCbor: encodeCbor([
      Buffer.concat([
        redeemerItem("Spend", 0n, Buffer.from(Data.to(0n), "hex")),
        Buffer.from([0x00]),
      ]),
    ]),
  });

/** A script envelope naming no language. */
const malformedScript = encodeCborArrayRaw([
  encodeCborUnsigned(99n),
  encodeCborBytes(Buffer.alloc(1)),
]);

/** A script witness whose envelope names no language. */
const malformedScriptWitness = () =>
  withFields({ scriptTxWitsPreimageCbor: encodeCbor([malformedScript]) });

/** An output whose reference script envelope names no language. */
const malformedOutputReferenceScript = () =>
  withFields({
    outputsPreimageCbor: encodeCbor([
      encodeCborMapRaw([
        [
          encodeCborUnsigned(0n),
          encodeCborBytes(protectedScriptAddress(receiver)),
        ],
        [encodeCborUnsigned(1n), value],
        [encodeCborUnsigned(3n), malformedScript],
      ]),
    ]),
  });

/** An output whose map keys are out of canonical order. */
const nonCanonicalOutput = () =>
  withFields({
    outputsPreimageCbor: encodeCbor([
      encodeCborMapRaw([
        [encodeCborUnsigned(1n), value],
        [
          encodeCborUnsigned(0n),
          encodeCborBytes(protectedScriptAddress(receiver)),
        ],
      ]),
    ]),
  });

describe("missingRedeemer under a later recorded reason", () => {
  it("detects a missing redeemer under a recorded PlutusExecutionFailed", async () => {
    await expect(replayUnder(plutusExecutionFailed)).resolves.toEqual([
      "redeemer-missing:1:0:2:0",
    ]);
  });

  it("detects a missing redeemer under a recorded UnusedRedeemer", async () => {
    await expect(replayUnder(unusedRedeemer)).resolves.toEqual([
      "redeemer-missing:1:0:2:0",
    ]);
  });

  it("detects a missing redeemer under a recorded ValueNotPreserved", async () => {
    await expect(replayUnder(rejected("ValueNotPreserved"))).resolves.toEqual([
      "redeemer-missing:1:0:2:0",
    ]);
  });

  it("keeps the RedeemerMissing outcomes for the same transaction", async () => {
    await expect(replayUnder(redeemerMissingVerdict(2n, 0n))).resolves.toEqual(
      [],
    );
    await expect(replayUnder(redeemerMissingVerdict(2n, 1n))).resolves.toEqual([
      "redeemer-missing:1:0:claim",
    ]);
  });

  it.each([
    ["ScriptIntegrityHashMissing", rejected("ScriptIntegrityHashMissing")],
    ["NetworkIdMismatch", rejected("NetworkIdMismatch")],
    [
      "RedeemerMalformed",
      rejected({ RedeemerMalformed: { redeemer_index: 0n } }),
    ],
  ])(
    "does not detect a missing redeemer under a recorded %s",
    async (_, verdict) => {
      await expect(replayUnder(verdict)).resolves.toEqual([]);
    },
  );

  it("does not detect a transaction whose observer carries its redeemer", async () => {
    const present: MissingRedeemerSubjectShape = {
      ...shape,
      observers: shape.observers.map((observer) => ({
        ...observer,
        redeemer: true,
      })),
    };
    await expect(replayUnder(plutusExecutionFailed, present)).resolves.toEqual(
      [],
    );
  });

  describe.each([
    ["PlutusExecutionFailed", plutusExecutionFailed],
    ["UnusedRedeemer", unusedRedeemer],
  ])("recorded %s", (_, verdict) => {
    it.each([
      ["a non-canonical redeemer", nonCanonicalRedeemer],
      ["a malformed script witness", malformedScriptWitness],
      ["a non-canonical output", nonCanonicalOutput],
    ])(
      "yields no detection for a transaction carrying %s",
      async (__, build) => {
        const nativeTx = build();
        expect(nativeTx).not.toStrictEqual(
          buildMissingRedeemerSubject(shape).nativeTx,
        );
        await expect(replayOf(shape, nativeTx, verdict)).resolves.toEqual([]);
      },
    );

    // The purpose walk never opens an output's reference script.
    it("detects the missing redeemer beside an output whose reference script is malformed", async () => {
      await expect(
        replayOf(shape, malformedOutputReferenceScript(), verdict),
      ).resolves.toEqual(["redeemer-missing:1:0:2:0"]);
    });
  });

  describe("an unresolved replay prerequisite", () => {
    /** An accepted transaction paying one key-address output. */
    const payment = (() => {
      const base = decodingSubjectTransaction({ fee: 5_000n });
      return materializeMidgardNativeTxFromCanonical({
        version: base.version,
        validity: base.validity,
        body: {
          ...base.body,
          outputsPreimageCbor: encodeCbor([
            encodeCborMapRaw([
              [
                encodeCborUnsigned(0n),
                encodeCborBytes(Buffer.from(`60${"11".repeat(28)}`, "hex")),
              ],
              [encodeCborUnsigned(1n), value],
            ]),
          ]),
        },
        witnessSet: base.witnessSet,
      });
    })();

    it("detects the missing redeemer when the payment is committed after it", async () => {
      await expect(
        replayOf(
          shape,
          buildMissingRedeemerSubject(shape).nativeTx,
          plutusExecutionFailed,
          {
            additionalTransactions: [payment],
          },
        ),
      ).resolves.toEqual(["redeemer-missing:1:0:2:0"]);
    });

    // The payment is committed first, with its post-ledger equal to its
    // pre-ledger, so the ledger before the rejection cannot be replayed.
    it("surfaces the prerequisite when the payment's committed effect differs", async () => {
      const replay = replayOf(
        shape,
        buildMissingRedeemerSubject(shape).nativeTx,
        plutusExecutionFailed,
        {
          additionalTransactions: [payment],
          orderEvents: (events) => [...events].reverse(),
        },
      );
      await expect(replay).rejects.toBeInstanceOf(
        CanonicalReplayPrerequisiteError,
      );
      await expect(replay).rejects.toMatchObject({
        failures: [
          {
            eventKeyCbor: Data.to(
              {
                L2TransactionEventKey: {
                  tx_id: computeMidgardNativeTxId(payment).toString("hex"),
                },
              },
              SDK.EventKey,
            ),
            prerequisite: "prior_transition_effect",
          },
        ],
      });
    });
  });
});
