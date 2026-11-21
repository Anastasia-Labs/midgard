import { existsSync } from "node:fs";
import { readFile, rm } from "node:fs/promises";
import { join } from "node:path";

import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  drillDirectories,
  stackDrill,
} from "./honest-successor.stack-drill.test-support.js";
import { verifyJourneySuccessorStageEvidence } from "./honest-successor.stage.js";
import {
  closeServers,
  faultHold,
  headerOn,
  honestOperator,
  operator2,
} from "./honest-successor.test-support.js";

afterAll(() =>
  Promise.all(
    drillDirectories.map((path) => rm(path, { recursive: true, force: true })),
  ),
);
afterEach(closeServers);

/**
 * The fault's DA attestation as the stack successor reads it from L1: when
 * it was applied after the fault's block time, and whether the watcher's
 * removal landed before it, which decides observable 1 when our node only
 * ever reported the fault awaiting its DA (#925).
 */
describe("stack successor and the fault's DA attestation", () => {
  it("records how long after the fault's block time its DA attestation was applied", async () => {
    const drill = await stackDrill();
    const port = drill.port();
    await port.afterFaultLanded();
    drill.node.readyz = faultHold(drill.fault.headerHash);
    await drill.observedHold();
    // The committee's DA apply landed at slot 12, inside our node's shift;
    // the watcher removed the attested fault at slot 20.
    drill.appoints(operator2, 9);
    drill.appoints(honestOperator, 11);
    drill.attests(12);
    drill.removed(20);
    drill.node.readyz = { ready: true, reasons: [] };
    const ours = await headerOn(
      drill.predecessor.headerHash,
      honestOperator,
      3n,
    );
    await drill.commits(ours, [drill.sent[0]!.txId]);
    const faultAttestation = {
      blockTimeMs: Number(drill.fault.header.endTime),
      apply: {
        txHash: drill.attestationApplyTxHash,
        slot: 12,
        atMs: drill.at(12),
      },
      lagAfterBlockTimeMs: drill.at(12) - Number(drill.fault.header.endTime),
    };
    await expect(port.commit(drill.correction)).resolves.toEqual({
      successor: expect.objectContaining({ headerHash: ours.headerHash }),
      stage: { successorStage: "exercised" },
      faultAttestation,
    });
    expect(
      JSON.parse(await readFile(join(drill.directory, "refusal.json"), "utf8")),
    ).toMatchObject({ faultAttestation });
  });

  describe("our node only ever reported the fault awaiting its DA", () => {
    /**
     * Our node reports the fault `landed_block_awaiting_da` only (the drill's
     * default `/readyz`), its shift starts at slot 11 while the fault is the
     * tail, and `chain` scripts the fault's removal and DA apply.
     */
    const awaitingOnly = async (
      chain: (drill: Awaited<ReturnType<typeof stackDrill>>) => void,
    ) => {
      const drill = await stackDrill();
      const port = drill.port();
      await port.afterFaultLanded();
      await drill.observedHold("awaiting_da");
      drill.appoints(operator2, 9);
      drill.appoints(honestOperator, 11);
      chain(drill);
      drill.node.readyz = { ready: true, reasons: [] };
      const ours = await headerOn(
        drill.predecessor.headerHash,
        honestOperator,
        3n,
      );
      await drill.commits(ours, [drill.sent[0]!.txId]);
      return { drill, port, ours };
    };
    const removedBeforeAttestation = (
      drill: Awaited<ReturnType<typeof stackDrill>>,
      attestationApply: Readonly<{ slot: number }> | null,
    ) => ({
      successorStage: "held-awaiting-da-removed-before-attestation",
      successorStageEvidence: {
        removal: {
          txHash: drill.correction.removalTxHash,
          slot: 13,
          atMs: drill.at(13),
        },
        attestationApply:
          attestationApply === null
            ? null
            : {
                txHash: drill.attestationApplyTxHash,
                slot: attestationApply.slot,
                atMs: drill.at(attestationApply.slot),
              },
      },
    });

    it("passes when L1 shows the watcher's removal strictly before the fault's DA apply, with both as evidence", async () => {
      const { drill, port, ours } = await awaitingOnly((drill) => {
        drill.removed(13);
        drill.attests(15);
      });
      const stage = removedBeforeAttestation(drill, { slot: 15 });
      await expect(port.commit(drill.correction)).resolves.toMatchObject({
        successor: { headerHash: ours.headerHash },
        stage,
      });
      expect(
        JSON.parse(
          await readFile(join(drill.directory, "refusal.json"), "utf8"),
        ),
      ).toMatchObject({ successor: ours.headerHash, stage });
      // A resumed run reads the stage back with its successor.
      await expect(
        drill.port().commit(drill.correction),
      ).resolves.toMatchObject({ stage });
    });

    it("passes when L1 shows the watcher's removal and no DA apply of the fault at all", async () => {
      const { drill, port } = await awaitingOnly((drill) => {
        drill.removed(13);
      });
      const stage = removedBeforeAttestation(drill, null);
      await expect(port.commit(drill.correction)).resolves.toMatchObject({
        stage,
        faultAttestation: { apply: null, lagAfterBlockTimeMs: null },
      });
      expect(
        JSON.parse(
          await readFile(join(drill.directory, "refusal.json"), "utf8"),
        ),
      ).toMatchObject({ stage });
    });

    it.each([
      [
        "the fault's DA apply landed before the removal",
        (drill: Awaited<ReturnType<typeof stackDrill>>) => {
          drill.attests(12);
          drill.removed(13);
        },
      ],
      [
        "the fault's DA apply landed in the removal's slot",
        (drill: Awaited<ReturnType<typeof stackDrill>>) => {
          drill.removed(13);
          drill.attests(13);
        },
      ],
      [
        "L1 shows the fault spent by another transaction than the watcher's removal",
        (drill: Awaited<ReturnType<typeof stackDrill>>) => {
          drill.removed(13, { by: "cd".repeat(32) });
        },
      ],
    ] as const)("fails as unobserved when %s", async (_label, chain) => {
      const { drill, port } = await awaitingOnly(chain);
      await expect(port.commit(drill.correction)).rejects.toThrow(
        /Our node never reported landed_block_invalid for the fault .* while it was the tail; the refusal was not observed/u,
      );
      expect(existsSync(join(drill.directory, "refusal.json"))).toBe(false);
    });

    it("keeps the exercised stage when our node also held the fault invalid", async () => {
      const drill = await stackDrill();
      const port = drill.port();
      await port.afterFaultLanded();
      await drill.observedHold("awaiting_da");
      drill.node.readyz = faultHold(drill.fault.headerHash);
      await drill.observedHold();
      drill.appoints(operator2, 9);
      drill.appoints(honestOperator, 11);
      drill.removed(13);
      drill.node.readyz = { ready: true, reasons: [] };
      await drill.commits(
        await headerOn(drill.predecessor.headerHash, honestOperator, 3n),
        [drill.sent[0]!.txId],
      );
      await expect(port.commit(drill.correction)).resolves.toMatchObject({
        stage: { successorStage: "exercised" },
      });
    });
  });

  describe("the fault's tail ends at the watcher's removal, not at its DA apply", () => {
    /**
     * The fault is committed at slot 10, operator-2's shift holds it from
     * slot 9, and our node first reports it invalid at slot 13. With
     * `attested`, its DA apply lands at slot 12 first, as #925 has it: the
     * apply spends the commit's output and the watcher's removal later spends
     * the attested one.
     */
    const heldFault = async ({ attested }: { attested: boolean }) => {
      const drill = await stackDrill({ startSlot: 13 });
      const port = drill.port();
      await port.afterFaultLanded();
      drill.appoints(operator2, 9);
      if (attested) drill.attests(12);
      drill.node.readyz = faultHold(drill.fault.headerHash);
      await drill.observedHold();
      return { drill, port };
    };
    const successorCommitted = async (
      drill: Awaited<ReturnType<typeof stackDrill>>,
    ) => {
      drill.node.readyz = { ready: true, reasons: [] };
      const ours = await headerOn(
        drill.predecessor.headerHash,
        honestOperator,
        3n,
      );
      await drill.commits(ours, [drill.sent[0]!.txId]);
      return ours;
    };
    const faultTail = async (drill: Awaited<ReturnType<typeof stackDrill>>) =>
      JSON.parse(
        await readFile(join(drill.directory, "fault-tail.json"), "utf8"),
      ) as unknown;

    it.each([
      ["unattested", false],
      ["attested", true],
    ] as const)(
      "exercises our shift between the hold and the removal of an %s fault, measured to the removal",
      async (_label, attested) => {
        const { drill, port } = await heldFault({ attested });
        drill.appoints(honestOperator, 14);
        drill.removed(20);
        const ours = await successorCommitted(drill);
        await expect(port.commit(drill.correction)).resolves.toMatchObject({
          successor: { headerHash: ours.headerHash },
          stage: { successorStage: "exercised" },
        });
        expect(await faultTail(drill)).toMatchObject({
          toMs: drill.at(20),
          ourShifts: [{ startTime: drill.at(14), endTime: drill.at(19) }],
          removal: {
            txHash: drill.correction.removalTxHash,
            slot: 20,
            atMs: drill.at(20),
          },
          measured: {
            faultCommitToConvictionMs: drill.at(20) - drill.at(10),
            faultCommitToOurNextShiftMs: drill.at(14) - drill.at(10),
          },
        });
      },
    );

    it("records an attested fault removed before our first shift, with the watcher's removal as evidence", async () => {
      const { drill, port } = await heldFault({ attested: true });
      drill.removed(14);
      drill.appoints(honestOperator, 16);
      const ours = await successorCommitted(drill);
      const notReached = {
        successorStage: "not-reached-fault-removed-before-our-shift",
        successorStageEvidence: {
          removal: {
            txHash: drill.correction.removalTxHash,
            slot: 14,
            atMs: drill.at(14),
          },
          ourFirstShiftStartMs: drill.at(16),
          operator2ShiftEndMs: drill.at(14),
        },
      };
      await expect(port.commit(drill.correction)).resolves.toMatchObject({
        successor: { headerHash: ours.headerHash },
        stage: notReached,
      });
      expect(
        JSON.parse(
          await readFile(join(drill.directory, "refusal.json"), "utf8"),
        ),
      ).toMatchObject({ stage: notReached });
      expect(await faultTail(drill)).toMatchObject({
        ourShifts: [],
        measured: { faultCommitToConvictionMs: drill.at(14) - drill.at(10) },
      });
    });

    it("ends the tail where an attested fault left the queue, without the watcher's removal, and proves no removal", async () => {
      const { drill, port } = await heldFault({ attested: true });
      // Another transaction than the watcher's removal spent the attested
      // output at slot 14; our shift from slot 16 came after the fault left.
      drill.removed(14, { by: "cd".repeat(32) });
      drill.appoints(honestOperator, 16);
      await successorCommitted(drill);
      const failure = port.commit(drill.correction);
      await expect(failure).rejects.toThrow(
        /had no shift while the fault .* was the tail/u,
      );
      await expect(failure).rejects.toThrow(
        /no output of the fault spent by the watcher's removal/u,
      );
      expect(await faultTail(drill)).toMatchObject({
        toMs: drill.at(14),
        ourShifts: [],
        removal: null,
        measured: { faultCommitToConvictionMs: null },
      });
    });
  });
});

describe("successor stage in a family's result evidence", () => {
  const removal = { txHash: "ab".repeat(32), slot: 13 };
  const evidence = (
    attestationApply: Readonly<{ txHash: string; slot: number }> | null,
  ) => ({
    successorStage: "held-awaiting-da-removed-before-attestation",
    successorStageEvidence: {
      removal: { ...removal, atMs: 13_000 },
      attestationApply:
        attestationApply === null
          ? null
          : { ...attestationApply, atMs: attestationApply.slot * 1_000 },
    },
  });

  it("accepts the other stages as before, and a fault removed before its DA apply with that evidence", () => {
    for (const result of [
      {},
      { successorStage: "exercised" },
      evidence(null),
      evidence({ txHash: "a7".repeat(32), slot: 14 }),
    ])
      expect(() =>
        verifyJourneySuccessorStageEvidence(result, removal),
      ).not.toThrow();
  });

  it.each([
    [
      "no evidence",
      { successorStage: "held-awaiting-da-removed-before-attestation" },
    ],
    [
      "a removal other than the result's correction",
      {
        ...evidence(null),
        successorStageEvidence: {
          ...evidence(null).successorStageEvidence,
          removal: { txHash: "cd".repeat(32), slot: 13, atMs: 13_000 },
        },
      },
    ],
    [
      "a removal slot other than the chain's",
      {
        ...evidence(null),
        successorStageEvidence: {
          ...evidence(null).successorStageEvidence,
          removal: { ...removal, slot: 12, atMs: 12_000 },
        },
      },
    ],
    [
      "a DA apply in the removal's slot",
      evidence({ txHash: "a7".repeat(32), slot: 13 }),
    ],
    [
      "a DA apply before the removal",
      evidence({ txHash: "a7".repeat(32), slot: 12 }),
    ],
    ["a malformed DA apply", evidence({ txHash: "not a hash", slot: 14 })],
  ] as const)(
    "refuses the stage of a fault removed before its DA apply with %s",
    (_label, result) => {
      expect(() =>
        verifyJourneySuccessorStageEvidence(result, removal),
      ).toThrow(/held-awaiting-da-removed-before-attestation/u);
    },
  );
});
