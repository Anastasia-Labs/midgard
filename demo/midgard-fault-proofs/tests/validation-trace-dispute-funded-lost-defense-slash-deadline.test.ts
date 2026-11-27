/** The deadline of the slash of a defender who lost a thread of our case,
 * under the watcher's prover funding, on the emulator. Past the defender's
 * bond unlock the slash is no longer due: a slash that did not build, or
 * that our wallet could not fund, raises no alarm once the unlock passes,
 * and the run stops under defender-won. A node that names no unlock stands
 * on the case's bound; when neither can be read, the slash is not due, and
 * the failure is logged once. A wallet run below the collateral line
 * before the slash raises the slash's alarm, naming the bond unlock. A
 * slash another watcher lands while our process is stopped at our own
 * submit is found after the restart, and the run stops cleanly. */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

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
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/validation-trace-dispute/wallet-move-unfunded-alarm.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { drainProverToOneCoin } from "./support/installed-prover-wallet.js";
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
  ADA,
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
    expect(
      (await confirmed(journey.dispute)).map(({ stage }) => stage),
    ).not.toContain("slash_lost_defense");
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
    },
    TIMEOUT,
  );

  it(
    "raises no alarm for a slash our wallet could not fund once the defender's bond unlock has passed, and stops under defender-won",
    async () => {
      const journey = await openTwoDefendedFundedCase();
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
    },
    TIMEOUT,
  );

  it(
    "raises the slash's alarm, naming its deadline, the defender's bond unlock, when our wallet runs below the collateral line before the slash",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      const [node] = await defenderNodes(journey);
      const unlock = nodeBondUnlock(node!)!;
      // Once our close of the thread is sent, the operator leaves one coin
      // below the collateral line, before the slash it settles.
      let drained = false;
      const draining: AdapterWrap = (adapter) => ({
        ...adapter,
        submit: async (input) => {
          const submitted = await adapter.submit(input);
          if (!drained && input.action.actionId.startsWith(CLOSE_THREAD)) {
            drained = true;
            await drainProverToOneCoin(journey.scene.h as never, 1n * ADA);
          }
          return submitted;
        },
      });
      const { result, history } = await playToDefenderWon(
        journey,
        draining,
        VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
      );
      expect(drained, history.join("\n")).toBe(true);
      expect(validationTraceRunState(result)).toMatchObject({
        namedState: {
          name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
          alarm: true,
        },
      });
      expect("reason" in result && result.reason).toContain(
        `, which must land before its deadline ${unlock.toString()}, the defender's bond unlock;`,
      );
      // The wallet's own refusal names the collateral it cannot cover.
      expect("reason" in result && result.reason).toMatch(
        /cover the required \d+ Lovelace collateral/u,
      );
      expect(
        (await confirmed(journey.dispute)).map(({ stage }) => stage),
      ).not.toContain("slash_lost_defense");
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
      await playToDefenderWon(
        journey,
        undefined,
        VALIDATION_TRACE_MOVE_UNBUILT,
      );
      const stages = await dispute.confirmed();
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
          receipts: [{ outRef: outRefOf(receipt!), utxo: receipt! }],
          now,
        });
      // The case's bound stands for the unlock the node does not name.
      expect(await due(dispute.caseHistory, bound - 1)).toMatchObject({
        kind: "due",
      });
      expect(await due(dispute.caseHistory, bound)).toBeUndefined();
      expect(warned).toStrictEqual([]);
      // The read holds the receipt alongside the case token, and returns
      // the case token's history alone.
      const asked: Parameters<typeof dispute.caseHistory.transactions>[0][] =
        [];
      const recording: typeof dispute.caseHistory = {
        transactions: async (input) => {
          asked.push(input);
          return await dispute.caseHistory.transactions(input);
        },
      };
      expect(await due(recording, bound - 1)).toMatchObject({ kind: "due" });
      const receiptUnit = Object.keys(receipt!.assets).find((unit) =>
        unit.startsWith(dispute.deployment.chain.award.policyId),
      );
      expect(receiptUnit).toBeDefined();
      expect(asked[0]).toStrictEqual({
        headerHash,
        caseUnit,
        alongside: [receiptUnit],
      });
      const hashes = (
        transactions: readonly Readonly<{ txHash: string }>[],
      ): string[] => transactions.map(({ txHash }) => txHash);
      expect(
        hashes(await dispute.caseHistory.transactions(asked[0]!)),
      ).toStrictEqual(
        hashes(
          await dispute.caseHistory.transactions({ headerHash, caseUnit }),
        ),
      );
      // A node whose datum cannot be read stands on the case's bound too,
      // logged once.
      const garbled = validationTraceLostDefenseSlashDeadlines({
        lucid: rewritingNodes(scene.h.proverLucid, () => Data.to(0n)),
        deployment: dispute.deployment,
        caseHistory: dispute.caseHistory,
        warn: (message) => warned.push(message),
      });
      for (let read = 0; read < 2; read += 1)
        expect(await garbled.ofReceipt(headerHash, receiptOutRef)).toBe(bound);
      expect(warned).toHaveLength(1);
      expect(warned[0]).toContain("the defender's bond unlock cannot be read");
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
            receipts: [{ outRef: outRefOf(receipt!), utxo: receipt! }],
            now: 0,
          }),
        ).toBeUndefined();
      expect(warned).toHaveLength(1);
      expect(warned[0]).toContain(`by receipt ${receiptOutRef}:`);
      expect(warned[0]).toContain("the case history is unreachable");
    },
    TIMEOUT,
  );

  it(
    "stops cleanly, with no alarm, after a restart that finds another watcher's slash landed while our process was stopped at our own submit",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      const { dispute } = journey;
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
      dispute.restart();
      const { result, alarms, history } = await turnCollecting(journey);
      expect(alarms, history.join("\n")).toStrictEqual([]);
      expect(validationTraceRunState(result)).toMatchObject({
        namedState: { name: VALIDATION_TRACE_DEFENDER_WON, alarm: false },
      });
      expect(
        (await confirmed(journey.dispute)).map(({ stage }) => stage),
      ).not.toContain("slash_lost_defense");
      expect(await defenderNodes(journey)).toStrictEqual([]);
    },
    TIMEOUT,
  );
});
