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
 *   the next survey after it is back answers; with no payload the survey
 *   looks up no ledger and reads no ledger row;
 * - a parent ledger the node's store cannot give (not retained, or the store
 *   not started) makes the material unavailable and keeps nothing, and the
 *   next survey with it held answers; a store retaining only the frontier's
 *   ledger answers with the same material, by the processed deltas;
 * - a case whose history is past the follower's retention is named as such,
 *   a detail that never fails readiness, even at its deadline;
 * - a case on a block past the confirmed frontier is not answered.
 */
import { MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS } from "@al-ft/midgard-core";
import {
  createMemoryLedgerStorage,
  LedgerNotRetained,
  type LedgerStorage,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  completeCanonicalReplayPredecessorStoreLedger,
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
import { buildValidationTraceDefenseMaterial } from "@al-ft/midgard-fault-proofs/validation-trace-dispute/defender";
import type { SqlClient } from "@effect/sql";
import { generateEmulatorAccount, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Metric } from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { DaPayloadsDB } from "../src/database/index.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { decideValidationTraceDefense } from "../src/validation-trace-defense/decide.js";
import { validationTraceDefenseChain } from "../src/validation-trace-defense/discovery.js";
import {
  initialValidationTraceDefenseHealth,
  validationTraceDefenseReadiness,
} from "../src/validation-trace-defense/health.js";
import type { DefenseLedgers } from "../src/validation-trace-defense/material.js";
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
import {
  frontierAtStart,
  ledgersOf,
  retain,
  sceneLedgerStore,
  seedLanded,
} from "./helpers/validation-trace-defense-scene.js";

const databases = testDatabases();
let env: IntentEmulator;
let scene: Scene;
let falseClaim: FalseClaim;
let faultCase: UTxO;
/** The node's ledger store, holding every ledger of the scene's blocks. */
let ledgers: DefenseLedgers;

const inNode = <A, E>(effect: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  env.runtime.runPromise(effect);

const survey = async (
  cache: DefenseMaterialCache = new Map(),
  withLedgers: DefenseLedgers = ledgers,
): Promise<DefenseSurvey> => {
  await env.follow();
  return await inNode(
    surveyValidationTraceDefense({
      chain: validationTraceDefenseChain(scene.chain),
      ourVkh: scene.operator.vkey,
      l1SlotConfig,
      cache,
      ledgers: withLedgers,
    }),
  );
};

const fullScans = Metric.value(fullScanCounter).pipe(
  Effect.map(({ count }) => count),
);

/** The material the fault-proofs package builds directly from the passing
 * block, at the false claim's step count. */
const directPassingMaterial = async () => {
  const direct = await buildValidationTraceDefenseMaterial({
    evidence: scene.passing.evidence,
    predecessor: completeCanonicalReplayPredecessorStoreLedger(scene.passing),
    eventKey: scene.passing.eventKey,
    stepCount: falseClaim.stepCount,
    l1SlotConfig,
  });
  if (direct.kind !== "defend") throw new Error("expected defense material");
  return direct;
};

/** Expects `found` to answer the false claim with `direct`'s material. */
const expectAnswered = (
  found: DefenseSurvey,
  direct: Awaited<ReturnType<typeof directPassingMaterial>>,
) => {
  const outcome = caseOn(found, falseClaim.caseUnit)?.outcome;
  if (outcome?.kind !== "defend")
    throw new Error(
      `expected defense material, got ${JSON.stringify(outcome, (_, value: unknown) => (typeof value === "bigint" ? value.toString() : value))}`,
    );
  expect(outcome.material.root).toBe(direct.root);
  expect(outcome.material.stateAtS).toStrictEqual(direct.stateAtS);
  expect(outcome.material.proofAtS).toStrictEqual(direct.proofAtS);
};

/** `storage` with its node and output reads counted. */
const countingStorage = (storage: LedgerStorage) => {
  const count = { reads: 0 };
  return {
    count,
    storage: {
      ...storage,
      readNodes: async (hashes) => {
        count.reads += hashes.length;
        return await storage.readNodes(hashes);
      },
      readOutputs: async (outRefs) => {
        count.reads += outRefs.length;
        return await storage.readOutputs(outRefs);
      },
    } satisfies LedgerStorage,
  };
};

/** `inner` with its lookups counted. */
const countingLedgers = (inner: DefenseLedgers) => {
  const count = { lookups: 0 };
  return {
    count,
    ledgers: {
      retainedLedger: (root) =>
        Effect.suspend(() => {
          count.lookups += 1;
          return inner.retainedLedger(root);
        }),
    } satisfies DefenseLedgers,
  };
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
  await inNode(seedLanded(scene));
  ledgers = ledgersOf(await sceneLedgerStore(scene));
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
  await inNode(frontierAtStart(scene));
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
      predecessor: completeCanonicalReplayPredecessorStoreLedger(scene.passing),
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

  it("looks up no ledger and reads no ledger row for a case whose block has no retained payload", async () => {
    const counting = countingStorage(createMemoryLedgerStorage());
    const store = await sceneLedgerStore(scene, { storage: counting.storage });
    const lookups = countingLedgers(ledgersOf(store));
    const blocks = [scene.passing, scene.failing];
    for (const { evidence } of blocks)
      await inNode(
        DaPayloadsDB.removeByHeaderHash(
          Buffer.from(evidence.headerHash, "hex"),
        ),
      );
    try {
      const scansBefore = await inNode(fullScans);
      const readsBefore = counting.count.reads;
      const found = await survey(new Map(), lookups.ledgers);
      expect(found.cases.map(({ outcome }) => outcome?.kind)).toStrictEqual([
        "unavailable",
        "unavailable",
      ]);
      expect(lookups.count.lookups).toBe(0);
      expect(counting.count.reads - readsBefore).toBe(0);
      expect((await inNode(fullScans)) - scansBefore).toBe(0);
    } finally {
      for (const block of blocks) await inNode(retain(block));
    }
    // With the payloads back, the same port is looked up and read.
    const scansBefore = await inNode(fullScans);
    const back = await survey(new Map(), lookups.ledgers);
    expectAnswered(back, await directPassingMaterial());
    expect(lookups.count.lookups).toBeGreaterThan(0);
    expect(counting.count.reads).toBeGreaterThan(0);
    expect((await inNode(fullScans)) - scansBefore).toBe(0);
  });

  it("makes the material unavailable and keeps nothing while the store cannot give the parent ledger, and answers once it can", async () => {
    const direct = await directPassingMaterial();
    for (const refusing of [
      (root: string) => Effect.fail(new LedgerNotRetained(root)),
      () =>
        Effect.fail(
          new Error(
            "The native MPF owner, which holds the ledger store, is not started",
          ),
        ),
    ]) {
      const cache: DefenseMaterialCache = new Map();
      const found = await survey(cache, { retainedLedger: refusing });
      for (const { outcome } of found.cases)
        expect(outcome).toMatchObject({
          kind: "unavailable",
          detail: expect.stringMatching(/parent ledger of block/u),
        });
      expect(cache.size).toBe(0);
      const state = decideValidationTraceDefense(found, Date.now()).states.find(
        ({ subject }) => subject === falseClaim.caseUnit,
      );
      expect(state?.state).toBe(
        "validation-trace-defense-material-unavailable",
      );
      expectAnswered(await survey(cache), direct);
    }
  });

  it("answers with the same material from a store that retains only the frontier's ledger", async () => {
    const frontierRoot = scene.failing.evidence.header.prevUtxosRoot;
    const store = await sceneLedgerStore(scene, { roots: [frontierRoot] });
    await expect(
      store.at(scene.passing.evidence.header.prevUtxosRoot),
    ).rejects.toBeInstanceOf(LedgerNotRetained);
    const found = await survey(new Map(), ledgersOf(store));
    expectAnswered(found, await directPassingMaterial());
    const faulted = found.cases.find(
      ({ target }) => target.headerHash === scene.failing.evidence.headerHash,
    )!;
    expect(faulted.outcome?.kind).toBe("operator_fault");
  });

  it("names a case whose history is past the follower's retention a detail that never fails readiness, even at its deadline", async () => {
    const found = await survey();
    // The survey of a case whose history the follower no longer holds.
    const pruned: DefenseSurvey = {
      ...found,
      cases: found.cases.map((candidate) =>
        candidate.caseUnit === falseClaim.caseUnit
          ? { ...candidate, priorDefenders: undefined, defended: undefined }
          : candidate,
      ),
    };
    const deadline =
      Number(caseOn(found, falseClaim.caseUnit)!.datum.started_at) +
      MIDGARD_VALIDATION_DISPUTE_SIDE_CLOCK_MS;
    const { states, work } = decideValidationTraceDefense(pruned, deadline - 1);
    expect(
      work.some(
        (item) =>
          item.kind === "defend" &&
          item.defenseCase.caseUnit === falseClaim.caseUnit,
      ),
    ).toBe(false);
    const named = states.filter(
      ({ subject }) => subject === falseClaim.caseUnit,
    );
    expect(named).toMatchObject([
      {
        state: "validation-trace-defense-history-pruned",
        deadline,
        failsReadiness: false,
      },
    ]);
    const { reasons, details } = validationTraceDefenseReadiness(
      {
        ...initialValidationTraceDefenseHealth(deadline - 1),
        following: true,
        states: named,
      },
      deadline - 1,
    );
    expect(reasons).toStrictEqual([]);
    expect(details).toStrictEqual([
      `validation-trace-defense-history-pruned:${falseClaim.caseUnit}:${deadline.toString()}`,
    ]);
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
