/**
 * A deposit whose L2 datum is deeply nested and spelled in the non-canonical
 * encodings Plutus Data admits (definite lists and constructor fields,
 * indefinite maps). The node's ingestion and the watcher's replay derive the
 * same L2 output from it, with the datum in its canonical spelling, and Phase
 * B in verdict mode gives a spend of that output its script's own verdict.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { replaceCborItemOnce } from "@al-ft/midgard-test-support/deep-plutus-data";
import {
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import {
  deepSpend,
  programScriptHash,
  shallowSpend,
  walkingFailingProgram,
  walkingProgram,
} from "@al-ft/midgard-validation/tests/phase-b.deep-data.fixture";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { readUserEvent } from "midgard-node/l1-events/entries";
import { beforeAll, describe, expect, it } from "vitest";

import { watcherReplayMaterial } from "../../src/l1-follower/replay-material.js";
import type { WatcherUserEvent } from "../../src/verification/user-event.js";

beforeAll(() => ledgerEd25519SignatureReady());

type Program = ReturnType<typeof walkingProgram>;

/** A datum nested `depth` levels deep: as sent on L1, and canonically. */
type OddDatum = Readonly<{ label: string; l1: string; canonical: string }>;

const oddDatums = (): readonly OddDatum[] => [
  {
    label: "8000 definite lists",
    l1: `${"81".repeat(8_000)}00`,
    canonical: `${"9f".repeat(8_000)}00${"ff".repeat(8_000)}`,
  },
  {
    label: "4000 definite constructor fields",
    l1: `${"d87981".repeat(4_000)}00`,
    canonical: `${"d8799f".repeat(4_000)}00${"ff".repeat(4_000)}`,
  },
  {
    label: "8000 indefinite maps",
    l1: `${"bf00".repeat(8_000)}00${"ff".repeat(8_000)}`,
    canonical: `${"a100".repeat(8_000)}00`,
  },
  {
    // A definite list of a definite constructor of an indefinite map, 2000
    // times over: 6000 levels.
    label: "6000 levels of definite lists, constructors and indefinite maps",
    l1: `${"81d87981bf00".repeat(2_000)}00${"ff".repeat(2_000)}`,
    canonical: `${"9fd8799fa100".repeat(2_000)}00${"ffff".repeat(2_000)}`,
  },
];

const MARKER = "c0ffee".repeat(10);
const eventId = { transactionId: "cd".repeat(32), outputIndex: 0n };
const idCbor = Data.to(eventId, SDK.OutputReference);
const originalAssetsCbor = Data.to(
  new Map([["", new Map([["", 10_000_000n]])]]) as SDK.Value,
  SDK.Value,
);

/** A deposit to the address `program` locks, carrying `l1Datum` as sent. */
const depositEvent = (program: Program, l1Datum: string): string =>
  replaceCborItemOnce(
    Data.to(
      {
        id: eventId,
        info: {
          l2_address: {
            paymentCredential: {
              ScriptCredential: [programScriptHash(program)],
            },
            stakeCredential: null,
          },
          l2_network_id: 0n,
          l2_datum: MARKER,
        },
      },
      SDK.DepositEvent,
    ),
    `581e${MARKER}`,
    l1Datum,
  );

/** The node's ingestion row for the deposit. */
const nodeIngests = (eventCbor: string) => {
  const read = readUserEvent(
    {
      kind: "deposit",
      key: "00",
      idCbor,
      inclusionTime: 5n,
      payloadCbor: `d8799f${eventCbor}ff`,
      originalAssetsCbor,
      admission: { outRef: { txHash: Buffer.alloc(32, 0xee), index: 0 } },
    } as unknown as ProjectedEvent,
    "Preprod",
  );
  if (read.kind !== "readable" || read.entry.kind !== "deposit")
    throw new Error(`the node did not ingest the deposit: ${read.kind}`);
  return read.entry.entry;
};

/** The watcher's replay of the committed deposit step the node derived. */
const watcherReplays = async (eventCbor: string, infoCbor: string) => {
  const material = watcherReplayMaterial({
    network: "Preprod",
    header: { startTime: 0n, endTime: 10n } as SDK.Header,
    blockTimeMs: 2,
    event: async () =>
      ({
        kind: "deposit",
        inclusionTime: "5",
        eventCborHex: eventCbor,
        originalAssetsCborHex: originalAssetsCbor,
      }) as unknown as WatcherUserEvent,
  });
  const replayed = await Effect.runPromise(
    material.replayUserEvent({
      step: { phase: "Deposit" } as SDK.TransitionStep,
      source: [idCbor, infoCbor],
      ledger: new Map(),
    }),
  );
  expect(replayed).toHaveLength(1);
  const [{ key, output }] = replayed as [{ key: string; output: Buffer }];
  return { outRef: Buffer.from(key, "hex"), output };
};

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
  cekExecutionMode: "verdict",
};

const phaseB = async (spend: ReturnType<typeof shallowSpend>) => {
  const result = await Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, config),
    ),
  );
  if (Either.isLeft(result)) throw new Error(String(result.left));
  return {
    accepted: result.right.accepted.length,
    rejected: result.right.rejected.map((rejected) => rejected.code),
  };
};

/** The deposited output both sides derive, locked by `program`. */
const depositedOutput = async (program: Program, datum: OddDatum) => {
  const eventCbor = depositEvent(program, datum.l1);
  const ingested = nodeIngests(eventCbor);
  const replayed = await watcherReplays(eventCbor, ingested.infoCbor);
  expect(replayed.output.toString("hex")).toBe(ingested.ledgerOutput);
  expect(
    decodeMidgardTxOutput(replayed.output).datum?.cbor.toString("hex"),
  ).toBe(datum.canonical);
  expect(replayed.output.length).toBeLessThanOrEqual(
    MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
  );
  return replayed;
};

describe("a deep, oddly encoded deposit datum through ingestion, replay and Phase B", () => {
  it.each(oddDatums())(
    "$label: node and watcher derive one output, and its spend gets its script's verdict",
    async (datum) => {
      const accepting = await depositedOutput(walkingProgram(), datum);
      expect(
        await phaseB(deepSpend(0x51, walkingProgram(), { spent: accepting })),
      ).toEqual({ accepted: 1, rejected: [] });
      const failing = await depositedOutput(walkingFailingProgram(), datum);
      expect(
        await phaseB(
          deepSpend(0x52, walkingFailingProgram(), { spent: failing }),
        ),
      ).toEqual({ accepted: 0, rejected: [RejectCodes.PlutusScriptInvalid] });
      expect(await phaseB(shallowSpend(0x53))).toEqual({
        accepted: 1,
        rejected: [],
      });
    },
    120_000,
  );
});
