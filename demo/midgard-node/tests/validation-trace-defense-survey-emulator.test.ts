/**
 * The defending operator's survey on the real deployment in the emulator,
 * with the node's follower attached before the blocks were committed and
 * the node's landed rows and retained payloads holding the scene's three
 * blocks:
 *
 * - a case opened on the passing block is found by its token and answered
 *   with the material the fault-proofs package builds directly from the
 *   block; an output at the case address carrying the same datum and no
 *   token is never a case;
 * - a case opened on the failing block reports the operator's own fault, a
 *   named detail that never fails readiness;
 * - a retained payload that is missing makes the material unavailable, and
 *   the next survey after it is back answers;
 * - a case on a block past the confirmed frontier is not answered.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import { buildValidationTraceDefenseMaterial } from "@al-ft/midgard-fault-proofs";
import {
  type Block,
  completeCanonicalReplayPredecessorLedger,
  failingClaim,
  failingMaterial,
  type FalseClaim,
  falseClaimMaterial,
  honestPresentation,
  initAndOpen,
  l1SlotConfig,
  openFalseClaim,
  type Scene,
  setUpScene,
  verifySource,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import type { SqlClient } from "@effect/sql";
import { generateEmulatorAccount, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import { foreignRetainedDaInsert } from "../src/da/foreign-retained-da.js";
import { ConfirmedLedgerDB, DaPayloadsDB } from "../src/database/index.js";
import {
  ledgerMap,
  ledgerRows,
  netDelta,
} from "../src/landed-blocks/ledger.js";
import { Frontier, insertRow } from "../src/landed-blocks/store.js";
import { decideValidationTraceDefense } from "../src/validation-trace-defense/decide.js";
import { validationTraceDefenseChain } from "../src/validation-trace-defense/discovery.js";
import {
  type DefenseMaterialCache,
  type DefenseSurvey,
  surveyValidationTraceDefense,
} from "../src/validation-trace-defense/survey.js";
import {
  attachIntentFollower,
  type IntentEmulator,
} from "./helpers/intent-journal-emulator.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const databases = testDatabases();
let env: IntentEmulator;
let scene: Scene;
let falseClaim: FalseClaim;
let faultCase: UTxO;

type Entry = Readonly<{ outRef: Buffer; output: Buffer }>;
const minimal = (entries: readonly Entry[]) =>
  entries.map(({ outRef, output }) => ({ outref: outRef, output }));

const inNode = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  env.runtime.runPromise(effect);

/**
 * Retains `block`'s payload as the node does. The fixture's body carries
 * no validation traces, while a retained row of a block with transactions
 * must count one per transaction under a non-empty root; the row takes
 * those from the header's counts and a stand-in root (the defense reads
 * only the payload).
 */
const retain = (block: Block) => {
  const { header, headerHash } = block.evidence;
  return DaPayloadsDB.upsertAvailable(
    foreignRetainedDaInsert(
      headerHash,
      header,
      header.endTime,
      Buffer.from(block.payloadEnvelopeCbor),
      { validationTracesRoot: "5a".repeat(32) },
      {
        validationTraceCount:
          header.forcedTransactionCount + header.l2TransactionCount,
      },
    ),
  );
};

/** The confirmed frontier at the failing block's parent. */
const frontierAtStart = Effect.suspend(() =>
  Frontier.upsert({
    headerHash: scene.failing.evidence.header.prevHeaderHash,
    utxosRoot: scene.failing.evidence.header.prevUtxosRoot,
  }),
);

/** The node's landed rows: the frontier at the failing block's parent,
 * then the scene's three blocks as processed own rows, each retained. */
const seedLanded = Effect.gen(function* () {
  const { failing, passing, forced } = scene;
  yield* frontierAtStart;
  yield* ConfirmedLedgerDB.insertMultiple(
    yield* ledgerRows(minimal(failing.predecessorEntries), new Map()),
  );
  for (const block of [failing, passing, forced]) {
    const { header, headerHash } = block.evidence;
    yield* insertRow({
      headerHash,
      parentHeaderHash: header.prevHeaderHash,
      parentUtxosRoot: header.prevUtxosRoot,
      utxosRoot: header.utxosRoot,
      kind: "own",
      state: "processed",
      applied: true,
      ...netDelta(
        ledgerMap(minimal(block.predecessorEntries)),
        minimal(block.postEntries),
      ),
      depositIds: [],
      withdrawals: [],
      forcedIds: [],
      txIds: [],
    });
    yield* retain(block);
  }
});

const survey = async (
  cache: DefenseMaterialCache = new Map(),
): Promise<DefenseSurvey> => {
  await env.follow();
  return await inNode(
    surveyValidationTraceDefense({
      chain: validationTraceDefenseChain(scene.chain),
      ourVkh: scene.operator.vkey,
      l1SlotConfig,
      cache,
    }),
  );
};

const caseOn = (found: DefenseSurvey, caseUnit: string) =>
  found.cases.find((candidate) => candidate.caseUnit === caseUnit);

/** Opens and verifies the honest claim on the failing block. */
const openFaultCase = async (): Promise<UTxO> => {
  const material = await failingMaterial(scene);
  const { opener } = await initAndOpen(
    scene,
    scene.failing,
    failingClaim(material),
  );
  const verification = verifySource(
    scene,
    opener,
    honestPresentation(material),
  );
  const signed = await (await verification.build()).sign
    .withWallet()
    .complete();
  const txHash = await signed.submit();
  await scene.h.proverLucid.awaitTx(txHash);
  const [caseUtxo] = await scene.h.proverLucid.utxosByOutRef([
    { txHash, outputIndex: 0 },
  ]);
  return caseUtxo!;
};

beforeAll(async () => {
  scene = await setUpScene();
  // Attached once the scene's submissions are observed (its raw L1 view
  // layers the emulator's submission), so this capture sees every later one.
  env = await attachIntentFollower(databases, {
    emulator: scene.h.emulator,
    own: scene.funder,
    payee: generateEmulatorAccount({ lovelace: 0n }),
    nodeSchema: true,
    protocol: { contracts: scene.h.contracts, referenceScriptAddresses: [] },
  });
  await inNode(seedLanded);
  falseClaim = await openFalseClaim(scene, await falseClaimMaterial(scene), 40);
  faultCase = await openFaultCase();
  // A look-alike: the false claim's case datum at the case address, with
  // no case token.
  const forged = await (
    await scene.h.proverLucid
      .newTx()
      .pay.ToContract(
        scene.chain.case.spendingScriptAddress,
        { kind: "inline", value: falseClaim.caseUtxo.datum! },
        { lovelace: 5_000_000n },
      )
      .complete()
  ).sign
    .withWallet()
    .complete();
  await scene.h.proverLucid.awaitTx(await forged.submit());
});

afterAll(async () => {
  await env.close();
  await databases.dropAll();
});

beforeEach(async () => {
  await inNode(frontierAtStart);
});

describe("the validation-trace defense survey", () => {
  it("answers the case on its block with the material built from the block, and never a look-alike", async () => {
    const found = await survey();
    expect(found.cases.map(({ caseUnit }) => caseUnit).sort()).toStrictEqual(
      [
        falseClaim.caseUnit,
        Object.keys(faultCase.assets).find((unit) =>
          unit.startsWith(scene.chain.case.policyId),
        )!,
      ].sort(),
    );
    const answered = caseOn(found, falseClaim.caseUnit)!;
    expect(answered.target.headerHash).toBe(scene.passing.evidence.headerHash);
    expect(answered.thread).toStrictEqual({ kind: "none" });
    expect(answered.defended).toBe(false);
    expect(answered.priorDefenders).toStrictEqual([]);
    const direct = await buildValidationTraceDefenseMaterial({
      evidence: scene.passing.evidence,
      predecessor: completeCanonicalReplayPredecessorLedger(scene.passing),
      eventKey: answered.datum.event_key,
      stepCount: falseClaim.stepCount,
      l1SlotConfig,
    });
    if (answered.outcome?.kind !== "defend" || direct.kind !== "defend")
      throw new Error("expected defense material from both");
    expect(answered.outcome.material.root).toBe(direct.root);
    expect(answered.outcome.material.stateAtS).toStrictEqual(direct.stateAtS);
    expect(answered.outcome.material.proofAtS).toStrictEqual(direct.proofAtS);
    const decision = decideValidationTraceDefense(found, Date.now());
    expect(
      decision.work
        .filter(({ kind }) => kind === "defend")
        .map((work) =>
          work.kind === "defend" ? work.defenseCase.caseUnit : undefined,
        ),
    ).toStrictEqual([falseClaim.caseUnit]);
  });

  it("reports its own fault on the failing block as a detail that never fails readiness", async () => {
    const found = await survey();
    const faulted = found.cases.find(
      ({ target }) => target.headerHash === scene.failing.evidence.headerHash,
    )!;
    expect(faulted.outcome?.kind).toBe("operator_fault");
    const deadline =
      Number(faulted.datum.started_at) +
      MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;
    const { states, work } = decideValidationTraceDefense(found, deadline - 1);
    expect(
      work.some(
        (item) =>
          item.kind === "defend" &&
          item.defenseCase.caseUnit === faulted.caseUnit,
      ),
    ).toBe(false);
    expect(
      states.find(({ subject }) => subject === faulted.caseUnit),
    ).toMatchObject({
      state: "validation-trace-defense-operator-fault",
      failsReadiness: false,
    });
  });

  it("makes the material unavailable while the block's payload is missing, and answers once it is back", async () => {
    const cache: DefenseMaterialCache = new Map();
    await inNode(
      DaPayloadsDB.removeByHeaderHash(
        Buffer.from(scene.passing.evidence.headerHash, "hex"),
      ),
    );
    const missing = await survey(cache);
    const lacking = caseOn(missing, falseClaim.caseUnit)!;
    expect(lacking.outcome?.kind).toBe("unavailable");
    const deadline =
      Number(lacking.datum.started_at) +
      MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;
    const early = decideValidationTraceDefense(
      missing,
      deadline - MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS / 2,
    );
    const late = decideValidationTraceDefense(missing, deadline - 1);
    for (const [decision, fails] of [
      [early, false],
      [late, true],
    ] as const)
      expect(
        decision.states.find(({ subject }) => subject === falseClaim.caseUnit),
      ).toMatchObject({
        state: "validation-trace-defense-material-unavailable",
        failsReadiness: fails,
      });
    await inNode(retain(scene.passing));
    const back = await survey(cache);
    expect(caseOn(back, falseClaim.caseUnit)!.outcome?.kind).toBe("defend");
  });

  it("does not answer a case on a block past the confirmed frontier", async () => {
    await inNode(
      Frontier.upsert({
        headerHash: scene.passing.evidence.headerHash,
        utxosRoot: scene.passing.evidence.header.utxosRoot,
      }),
    );
    const found = await survey();
    expect(caseOn(found, falseClaim.caseUnit)).toBeUndefined();
    expect(found.cases).toStrictEqual([]);
  });
});
