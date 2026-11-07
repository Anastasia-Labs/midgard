/** The deadline of the slash of a defender who lost a thread of our case,
 * under the watcher's prover funding, on the emulator. Past the defender's
 * bond unlock the slash is no longer due: a slash that did not build, or
 * that our wallet could not fund, raises no alarm once the unlock passes,
 * and the run stops under defender-won. A node that names no unlock stands
 * on the case's bound; when neither can be read, the slash is not due, and
 * the failure is logged once. A slash another watcher lands while our
 * process is stopped at our own submit is found after the restart, and the
 * run stops cleanly. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import {
  validationTraceCaseBondUnlockMs,
  validationTraceCaseStartedAt,
} from "../src/validation-trace-dispute/case-origin.js";
import {
  dueValidationTraceLostDefenseSlash,
  validationTraceLostDefenseSlashDeadlines,
} from "../src/validation-trace-dispute/lost-defense-slash-due.js";
import {
  VALIDATION_TRACE_DEFENDER_WON,
  VALIDATION_TRACE_MOVE_UNBUILT,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  confirmed,
  defenderNodes,
  landForeignSlash,
  nodeBondUnlock,
  outRefOf,
  playToDefenderWon,
  SLASH,
  turnCollecting,
  type TwoDefended,
  unpublishedActiveOperators,
} from "./support/validation-trace-dispute-funded-lost-defense.js";
import {
  caseUnitOf,
  openTwoDefendedFundedCase,
  successorWinsItsThread,
} from "./support/validation-trace-dispute-funded-moves.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import {
  entriesOf,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";

const CLOSE_THREAD = "validation-trace-close-thread:";

/** The emulator's clock moved past `unlockMs`. */
const passUnlock = ({ scene }: TwoDefended, unlockMs: bigint) => {
  const { h } = scene;
  const lucid = h.proverLucid;
  h.emulator.awaitSlot(
    lucid.unixTimeToSlot(Number(unlockMs)) - lucid.currentSlot() + 1,
  );
  expect(BigInt(h.emulator.now())).toBeGreaterThan(unlockMs);
};

/** Two more turns past the unlock: each stops under defender-won, with no
 * alarm and no slash attempted. */
const stopsCleanly = async (journey: TwoDefended) => {
  for (let turn = 0; turn < 2; turn += 1) {
    const result: FraudProofWorkflowRunResult = await journey.dispute.turn();
    expect(result.kind).toBe("pending");
    if (result.kind !== "pending") return;
    expect(result.reason.startsWith(`${VALIDATION_TRACE_DEFENDER_WON}:`)).toBe(
      true,
    );
    expect(result.alarms ?? []).toStrictEqual([]);
    expect(validationTraceRunState(result)).toMatchObject({
      kind: "pending",
      namedState: { name: VALIDATION_TRACE_DEFENDER_WON, alarm: false },
    });
    expect(confirmed(result).map(({ stage }) => stage)).not.toContain(
      "slash_lost_defense",
    );
  }
  // Nobody slashed the defender: its node stands.
  expect(await defenderNodes(journey)).toHaveLength(1);
};

/** `lucid`, its operator nodes read with `rewrite` applied to each datum. */
const rewritingNodes = (
  lucid: LucidEvolution,
  rewrite: (datum: string) => string,
): LucidEvolution =>
  new Proxy(lucid, {
    get: (target, property) => {
      if (property === "utxosAtWithUnit")
        return async (address: string, unit: string) =>
          (await target.utxosAtWithUnit(address, unit)).map(
            (utxo): UTxO =>
              unit
                .slice(56)
                .startsWith(SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX)
                ? { ...utxo, datum: rewrite(utxo.datum!) }
                : utxo,
          );
      const value: unknown = Reflect.get(target, property, target);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });

/** The node's datum naming no bond unlock. */
const noUnlock = (datum: string) => {
  const list = Data.from(datum, SDK.LinkedListDatum);
  if (!("Node" in list.data)) throw new Error("not a node");
  const operator = Data.from(
    Data.to(list.data.Node.data),
    SDK.ActiveOperatorDatum,
  );
  return Data.to(
    {
      ...list,
      data: {
        Node: {
          ...list.data.Node,
          data: Data.from(
            Data.to(
              { ...operator, bond_unlock_time: null },
              SDK.ActiveOperatorDatum,
            ),
          ),
        },
      },
    },
    SDK.LinkedListDatum,
  );
};

describe("the deadline of the slash of a defender who lost a thread of our case", () => {
  it(
    "raises no alarm for a slash that did not build once the defender's bond unlock has passed, and stops under defender-won",
    async () => {
      const journey = await openTwoDefendedFundedCase({
        rewriteRemoval: unpublishedActiveOperators,
      });
      try {
        const [node] = await defenderNodes(journey);
        const unlock = nodeBondUnlock(node!)!;
        const { result } = await playToDefenderWon(
          journey,
          undefined,
          VALIDATION_TRACE_MOVE_UNBUILT,
        );
        expect(validationTraceRunState(result)).toMatchObject({
          namedState: { name: VALIDATION_TRACE_MOVE_UNBUILT, alarm: true },
        });
        passUnlock(journey, unlock);
        await stopsCleanly(journey);
      } finally {
        await journey.dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "raises no alarm for a slash our wallet could not fund once the defender's bond unlock has passed, and stops under defender-won",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      try {
        const [node] = await defenderNodes(journey);
        const unlock = nodeBondUnlock(node!)!;
        // The ledger's collateral rate rises past what the wallet holds once
        // our close of the thread is sent, before the slash it settles.
        const { emulator } = journey.scene.h;
        const parameters = emulator.protocolParameters;
        let raised = false;
        const costly: AdapterWrap = (adapter) => ({
          ...adapter,
          submit: async (input) => {
            const submitted = await adapter.submit(input);
            if (input.action.actionId.startsWith(CLOSE_THREAD)) {
              raised = true;
              emulator.protocolParameters = {
                ...parameters,
                collateralPercentage: 1_000_000,
              };
            }
            return submitted;
          },
        });
        const { result, history } = await playToDefenderWon(
          journey,
          costly,
          VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
        );
        expect(raised, history.join("\n")).toBe(true);
        expect(validationTraceRunState(result)).toMatchObject({
          namedState: {
            name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
            alarm: true,
          },
        });
        expect("reason" in result && result.reason).toContain(
          `, which must land before its deadline ${unlock.toString()}, the defender's bond unlock;`,
        );
        // The rate stays raised: a slash proposed again would alarm again.
        passUnlock(journey, unlock);
        await stopsCleanly(journey);
      } finally {
        await journey.dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "takes the case's bound for a node that names no bond unlock, and holds no slash due, logging once, when that bound cannot be read either",
    async () => {
      const journey = await openTwoDefendedFundedCase({
        rewriteRemoval: unpublishedActiveOperators,
      });
      const { scene, dispute } = journey;
      try {
        const { result } = await playToDefenderWon(
          journey,
          undefined,
          VALIDATION_TRACE_MOVE_UNBUILT,
        );
        const stages = confirmedValidationTraceStages(entriesOf(result));
        const receiptOutRef = stages
          .find(({ action }) => action.stage === "close_thread")!
          .outputs.find(({ role }) => role === "receipt")!.outRef;
        const [receipt] = await scene.h.proverLucid.utxosByOutRef([
          {
            txHash: receiptOutRef.slice(0, 64),
            outputIndex: Number(receiptOutRef.slice(65)),
          },
        ]);
        const lucid = rewritingNodes(scene.h.proverLucid, noUnlock);
        const headerHash = scene.failing.evidence.headerHash;
        const caseUnit = caseUnitOf(journey);
        const bound = validationTraceCaseBondUnlockMs(
          validationTraceCaseStartedAt({
            transactions: await dispute.caseHistory.transactions({
              headerHash,
              caseUnit,
            }),
            caseUnit,
          }),
        );
        const warned: string[] = [];
        const due = async (
          caseHistory: typeof dispute.caseHistory,
          now: number,
        ) =>
          await dueValidationTraceLostDefenseSlash({
            lucid,
            signer: scene.h.proverSigner as never,
            deployment: dispute.deployment,
            headerHash,
            deadlines: validationTraceLostDefenseSlashDeadlines({
              lucid,
              deployment: dispute.deployment,
              caseHistory,
              warn: (message) => warned.push(message),
            }),
            stages,
            live: new Map([[outRefOf(receipt!), receipt!]]),
            now,
          });
        // The case's bound stands for the unlock the node does not name.
        expect(await due(dispute.caseHistory, bound - 1)).toMatchObject({
          kind: "due",
        });
        expect(await due(dispute.caseHistory, bound)).toBeUndefined();
        expect(warned).toStrictEqual([]);
        // A node whose datum cannot be read stands on the case's bound too,
        // logged once.
        const garbled = validationTraceLostDefenseSlashDeadlines({
          lucid: rewritingNodes(scene.h.proverLucid, () => Data.to(0n)),
          deployment: dispute.deployment,
          caseHistory: dispute.caseHistory,
          warn: (message) => warned.push(message),
        });
        for (let read = 0; read < 2; read += 1)
          expect(await garbled.ofReceipt(headerHash, receiptOutRef)).toBe(
            bound,
          );
        expect(warned).toHaveLength(1);
        expect(warned[0]).toContain(
          "the defender's bond unlock cannot be read",
        );
        warned.length = 0;
        // With the case's history unreadable, no deadline is known.
        const deadlines = validationTraceLostDefenseSlashDeadlines({
          lucid,
          deployment: dispute.deployment,
          caseHistory: {
            transactions: async () => {
              throw new Error("the case history is unreachable");
            },
          } as never,
          warn: (message) => warned.push(message),
        });
        for (let read = 0; read < 2; read += 1)
          expect(
            await dueValidationTraceLostDefenseSlash({
              lucid,
              signer: scene.h.proverSigner as never,
              deployment: dispute.deployment,
              headerHash,
              deadlines,
              stages,
              live: new Map([[outRefOf(receipt!), receipt!]]),
              now: 0,
            }),
          ).toBeUndefined();
        expect(warned).toHaveLength(1);
        expect(warned[0]).toContain(`by receipt ${receiptOutRef}:`);
        expect(warned[0]).toContain("the case history is unreachable");
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "stops cleanly, with no alarm, after a restart that finds another watcher's slash landed while our process was stopped at our own submit",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      const { dispute } = journey;
      try {
        await successorWinsItsThread(journey);
        let stopped!: () => void;
        const stop = new Promise<void>((resolve) => {
          stopped = resolve;
        });
        const foreign: string[] = [];
        const stopAtSubmit: AdapterWrap = (adapter) => ({
          ...adapter,
          submit: async (input) => {
            if (!input.action.actionId.startsWith(SLASH))
              return await adapter.submit(input);
            foreign.push(
              await landForeignSlash(
                journey,
                input.action.actionId.slice(SLASH.length),
              ),
            );
            stopped();
            return await new Promise<never>(() => undefined);
          },
        });
        await Promise.race([
          stop,
          turnCollecting(journey, stopAtSubmit).then(({ history }) => {
            throw new Error(
              `the process ended before submit: ${history.join("\n")}`,
            );
          }),
        ]);
        expect(foreign).toHaveLength(1);
        await dispute.restart();
        const { result, alarms, history } = await turnCollecting(journey);
        expect(alarms, history.join("\n")).toStrictEqual([]);
        expect(validationTraceRunState(result)).toMatchObject({
          namedState: { name: VALIDATION_TRACE_DEFENDER_WON, alarm: false },
        });
        expect(confirmed(result).map(({ stage }) => stage)).not.toContain(
          "slash_lost_defense",
        );
        expect(await defenderNodes(journey)).toStrictEqual([]);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
