/**
 * The defending operator's published references, as the node's fiber reads
 * them, on the real deployment in the emulator: each by its role, at the
 * deployment's reference-script address, under its role's authentication
 * token.
 *
 * - With every role published, the Defend lands.
 * - With a role's reference not published, a Defend that would read them
 *   is named `validation-trace-defense-references-unresolved`, with the
 *   role it lacks; nothing is built, the tick does not fail, and readiness
 *   holds while the deadline is far.
 */
import {
  falseClaimMaterial,
  openFalseClaim,
  publishAuthenticatedValidationDisputeControl,
  type Scene,
} from "@al-ft/midgard-fault-proofs/test-support/validation-trace-defender-scene";
import type { Script } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import { validationTraceDefenseReferences } from "../src/fibers/validation-trace-defense-fiber.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import {
  type DefenseScene,
  openDefenseScene,
} from "./helpers/validation-trace-defense-scene.js";

const databases = testDatabases();
const opened: DefenseScene[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map(({ env }) => env.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

type Publication = Parameters<
  typeof publishAuthenticatedValidationDisputeControl
>[0];
type Role = Publication["target"]["name"];

/** Every role the defender's moves read, and its script, in `scene`. */
const roles = ({ h, chain }: Scene): ReadonlyArray<readonly [Role, Script]> => [
  ["V1 validation-trace case", chain.case.spendingScript],
  ["V1 validation-trace game", chain.game.spendingScript],
  ["V1 validation-trace timeout", chain.timeout.spendingScript],
  ["V1 validation-trace boundary", chain.boundary.spendingScript],
  ["V1 validation-trace thread expiry", chain.threadExpiry.withdrawalScript],
  ["active-operators spending", h.contracts.activeOperators.spendingScript],
];

/**
 * The scene with every role's reference published under its token but
 * `missing`'s, a false claim on our passing block, and the port reading
 * the references as the fiber does.
 */
const openPublished = async (missing?: Role) => {
  const s = await openDefenseScene(databases, async (scene) => {
    for (const [name, script] of roles(scene))
      if (name !== missing)
        await publishAuthenticatedValidationDisputeControl({
          lucid: scene.h.proverLucid,
          authPolicy: scene.h.contracts
            .referenceScriptAuth as Publication["authPolicy"],
          publisher: scene.h.contracts.referenceScriptPublisher!,
          target: { name, control: name, script },
        });
  });
  opened.push(s);
  const { h } = s.scene;
  const claim = await openFalseClaim(
    s.scene,
    await falseClaimMaterial(s.scene),
    40,
  );
  await s.env.follow();
  const port = {
    ...s.port,
    references: validationTraceDefenseReferences(
      s.port.lucid,
      await h.contracts.referenceScriptPublisher!.lucid.wallet().address(),
      h.contracts,
    ),
  };
  return { s, claim, port };
};

describe("the validation-trace defender's published references", () => {
  it("lands the Defend with the references it finds by their roles", async () => {
    const { s, claim, port } = await openPublished();
    expect(await s.tick(port)).toStrictEqual([
      { kind: "defend", subject: claim.caseUnit, outcome: "landed" },
    ]);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  });

  it("names a Defend whose references lack a role, and builds nothing", async () => {
    const { s, claim, port } = await openPublished(
      "V1 validation-trace boundary",
    );
    const sentBefore = s.env.sent.length;
    expect(await s.tick(port)).toStrictEqual([]);
    expect(s.env.sent.length).toBe(sentBefore);
    const health = await s.health();
    const unresolved = health.states.find(
      ({ subject }) => subject === claim.caseUnit,
    );
    expect(unresolved).toMatchObject({
      state: "validation-trace-defense-references-unresolved",
      failsReadiness: false,
    });
    expect(unresolved!.detail).toContain("V1 validation-trace boundary");
    expect(health.dueDefends).toStrictEqual([]);
    expect(health.consecutiveFailures).toBe(0);
    expect((await s.readiness()).reasons).toStrictEqual([]);
  });
});
