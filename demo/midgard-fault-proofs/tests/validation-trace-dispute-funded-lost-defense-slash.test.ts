/** The slash of a defender who lost a thread of our case, under the
 * watcher's prover funding, on the emulator. Two operators defend our case:
 * the successor reveals and, no turn of ours answering before our clock on
 * its thread runs out, closes its thread into the case as its win; the
 * operator stays silent, and our timeout closes its thread into our case
 * and mints its lost-defense receipt. With the case settled for us, the
 * watcher's next action spends that receipt to slash the operator: its
 * node leaves the active set, the slashing penalty is the fee, the reward
 * pays our funding key, and the rest of the bond and the receipt's
 * lovelace stay with the directory anchor; our wallet only lends
 * collateral; the run then ends under defender-won. A slash another
 * watcher lands first leaves the watcher nothing to do and no alarm, and a
 * slash that does not build is the operator's alarm, naming the
 * defender's bond unlock it must land by, raised once the thread's close
 * has landed and the case is settled.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  getAddressDetails,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { fraudSlashEconomicsFromDeploymentManifest } from "../src/remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { captureValidationTraceLostDefenseSlash } from "../src/validation-trace-dispute/lost-defense-slash.js";
import {
  VALIDATION_TRACE_DEFENDER_WON,
  VALIDATION_TRACE_MOVE_UNBUILT,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { network } from "./support/submit-init-emulator-shared.js";
import { anotherParty } from "./support/validation-trace-dispute-foreign-verify.js";
import {
  caseUnitOf,
  type FundedMoves,
  openTwoDefendedFundedCase,
  recordedOutput,
  type SecondDefense,
  successorWinsItsThread,
} from "./support/validation-trace-dispute-funded-moves.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import {
  entriesOf,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import { liveCase } from "./support/validation-trace-dispute-one-step-case.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

type TwoDefended = FundedMoves & Readonly<{ second: SecondDefense }>;

const SLASH = "validation-trace-slash-lost-defense:";

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The defender's operator node, in whichever operator set holds it. */
const defenderNodes = async ({ scene, defender }: FundedMoves) => {
  const { h } = scene;
  const vkh = defender.vkey;
  return [
    ...(await h.proverLucid.utxosAtWithUnit(
      h.contracts.activeOperators.spendingScriptAddress,
      h.contracts.activeOperators.policyId +
        SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
        vkh,
    )),
    ...(await h.proverLucid.utxosAtWithUnit(
      h.contracts.retiredOperators.spendingScriptAddress,
      h.contracts.retiredOperators.policyId +
        SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX +
        vkh,
    )),
  ];
};

/** The defender's lost-defense receipt token. */
const receiptUnit = ({ scene, unit }: FundedMoves) =>
  scene.chain.award.policyId + unit.slice(56);

type CapturedSlash = Readonly<{
  signed: TxSigned;
  /** The prover wallet's outputs as the slash was built. */
  wallet: ReadonlyMap<string, UTxO>;
}>;

/** Records every slash the watcher builds, with its wallet as it was. */
const captureSlashes =
  (journey: FundedMoves, slashes: CapturedSlash[]): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const signed = workflowPreflightTransaction(preflight);
      if (context.action.actionId.startsWith(SLASH) && signed !== undefined) {
        const { h } = journey.scene;
        slashes.push({
          signed,
          wallet: new Map(
            (await h.emulator.getUtxos(h.proverSigner.address)).map(
              (utxo) => [outRefOf(utxo), utxo] as const,
            ),
          ),
        });
      }
      return preflight;
    },
  });

/**
 * The successor wins its thread while the operator stays silent past its
 * deadline, and the watcher turns, with
 * `wrap`, until the run is pending for `until` (defender-won), every alarm
 * each turn raised kept.
 */
const playToDefenderWon = async (
  journey: TwoDefended,
  wrap?: AdapterWrap,
  until = VALIDATION_TRACE_DEFENDER_WON,
) => {
  const { dispute } = journey;
  await successorWinsItsThread(journey);
  const alarms: string[] = [];
  const history: string[] = [];
  for (let turns = 0; turns < 12; turns += 1) {
    const result: FraudProofWorkflowRunResult = await dispute.turn(wrap);
    history.push(
      "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
    );
    if (result.kind === "pending") {
      alarms.push(...(result.alarms ?? []).map(({ reason }) => reason));
      if (result.reason.startsWith(until)) return { result, alarms, history };
    }
  }
  throw new Error(`the run never reached ${until}: ${history.join("\n")}`);
};

/** The stages the journal confirmed, with each transaction's hash. */
const confirmed = (result: FraudProofWorkflowRunResult) =>
  confirmedValidationTraceStages(entriesOf(result)).map(
    ({ action, txHash }) => ({ stage: action.stage, txHash }),
  );

describe("the slash of a defender who lost a thread of our case", () => {
  it(
    "slashes the operator by its receipt once its thread's close settles the case for us, then ends under defender-won: its node leaves the set, the penalty is the fee, the reward pays our funding key, the anchor keeps the rest, and our wallet only lends collateral",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      const { scene, dispute } = journey;
      try {
        const { h } = scene;
        const [node] = await defenderNodes(journey);
        expect(node).toBeDefined();
        // The operator directory and the scheduler as they stand before.
        const before = new Map(
          [
            ...(await h.emulator.getUtxos(
              h.contracts.activeOperators.spendingScriptAddress,
            )),
            ...(await h.emulator.getUtxos(
              h.contracts.scheduler.spendingScriptAddress,
            )),
          ].map((utxo) => [outRefOf(utxo), utxo] as const),
        );
        const slashes: CapturedSlash[] = [];
        const { result, alarms, history } = await playToDefenderWon(
          journey,
          captureSlashes(journey, slashes),
        );
        expect(alarms, history.join("\n")).toStrictEqual([]);
        const stages = confirmed(result);
        expect(stages.map(({ stage }) => stage).slice(-2)).toStrictEqual([
          "close_thread",
          "slash_lost_defense",
        ]);
        const slash = stages.find(
          ({ stage }) => stage === "slash_lost_defense",
        )!;
        expect(slashes.map(({ signed }) => signed.toHash())).toStrictEqual([
          slash.txHash,
        ]);
        const close = stages.find(({ stage }) => stage === "close_thread")!;
        const receipt = recordedOutput(scene, `${close.txHash}#1`);
        expect(receipt.assets[receiptUnit(journey)]).toBe(1n);

        // The defender is in neither set, and its receipt is burned.
        expect(await defenderNodes(journey)).toStrictEqual([]);
        expect(
          await h.proverLucid.utxosAtWithUnit(
            scene.chain.award.spendingScriptAddress,
            receiptUnit(journey),
          ),
        ).toStrictEqual([]);
        const body = slashes[0]!.signed.toTransaction().body();
        expect(
          body
            .mint()
            ?.get_assets(CML.ScriptHash.from_hex(scene.chain.award.policyId))
            ?.get(CML.AssetName.from_hex(journey.unit.slice(56))),
        ).toBe(-1n);

        // The slashing penalty is the fee; the reward pays our funding key.
        const economics = fraudSlashEconomicsFromDeploymentManifest(
          journey.removal.removal.deploymentInfo,
        );
        const bond = node!.assets.lovelace!;
        expect(bond).toBe(economics.requiredBondLovelace);
        expect(body.fee()).toBe(economics.slashingPenaltyLovelace);
        const outputs = Array.from(
          { length: body.outputs().len() },
          (_, index) => recordedOutput(scene, `${slash.txHash}#${index}`),
        );
        const rewardAddress = credentialToAddress(network, {
          type: "Key",
          hash: h.proverSigner.paymentKeyHash,
        });
        expect(
          outputs
            .filter(({ address }) => address === rewardAddress)
            .map(({ assets }) => assets),
        ).toStrictEqual([{ lovelace: economics.fraudProverRewardLovelace }]);
        // The slash spends the receipt, the defender's node, and only
        // directory and scheduler outputs besides: no wallet input.
        const inputs = body.inputs();
        const spent = Array.from({ length: inputs.len() }, (_, index) => {
          const input = inputs.get(index);
          return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
        });
        expect(spent).toContain(outRefOf(receipt));
        expect(spent).toContain(outRefOf(node!));
        for (const outRef of spent)
          expect(outRef === outRefOf(receipt) || before.has(outRef)).toBe(true);
        // The anchor, the directory output it continues, keeps the bond
        // beyond fee and reward, and the receipt's lovelace.
        const directoryUnit = (utxo: UTxO) =>
          Object.keys(utxo.assets).find((held) =>
            held.startsWith(h.contracts.activeOperators.policyId),
          );
        const anchorAfter = outputs.find(
          ({ address }) =>
            address === h.contracts.activeOperators.spendingScriptAddress,
        )!;
        const anchorBefore = spent
          .map((outRef) => before.get(outRef))
          .find(
            (utxo) =>
              utxo !== undefined &&
              directoryUnit(utxo) === directoryUnit(anchorAfter),
          )!;
        expect(anchorAfter.assets.lovelace).toBe(
          anchorBefore.assets.lovelace! +
            bond -
            economics.slashingPenaltyLovelace -
            economics.fraudProverRewardLovelace +
            receipt.assets.lovelace!,
        );
        const collateral = body.collateral_inputs();
        expect(collateral?.len()).toBe(1);
        const lent = slashes[0]!.wallet.get(
          `${collateral!.get(0).transaction_id().to_hex()}#${collateral!.get(0).index().toString()}`,
        )!;
        expect(Object.keys(lent.assets)).toStrictEqual(["lovelace"]);
        reportFit(
          "funded slash_lost_defense",
          slashes.map(({ signed }) => signed.toCBOR()),
        );
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "stops cleanly, with no alarm, when another watcher's slash of the defender lands first",
    async () => {
      const journey = await openTwoDefendedFundedCase();
      const { scene, dispute } = journey;
      try {
        const lucid = await anotherParty(scene);
        const address = await lucid.wallet().address();
        const other = {
          source: "direct-seed-phrase",
          address,
          paymentKeyHash: getAddressDetails(address).paymentCredential!.hash,
          selectWallet: () => undefined,
        };
        const foreign: string[] = [];
        // Another watcher lands its slash between our observation and our
        // build of ours.
        const race: AdapterWrap = (adapter) => ({
          ...adapter,
          preflight: async (context) => {
            if (
              context.action.actionId.startsWith(SLASH) &&
              foreign.length === 0
            ) {
              const receiptOutRef = context.action.actionId.slice(SLASH.length);
              const { transaction } =
                await captureValidationTraceLostDefenseSlash({
                  lucid,
                  signer: other,
                  deployment: dispute.deployment,
                  removal: journey.removal.removal,
                  headerHash: scene.failing.evidence.headerHash,
                  receiptOutRef,
                });
              foreign.push(await transaction.signed.submit());
              await lucid.awaitTx(foreign[0]!);
            }
            return await adapter.preflight(context);
          },
        });
        const { result, alarms, history } = await playToDefenderWon(
          journey,
          race,
        );
        expect(foreign, history.join("\n")).toHaveLength(1);
        expect(alarms, history.join("\n")).toStrictEqual([]);
        const stages = confirmed(result).map(({ stage }) => stage);
        expect(stages).not.toContain("slash_lost_defense");
        expect(stages.slice(-1)).toStrictEqual(["close_thread"]);
        expect(
          entriesOf(result).some(
            ({ event }) =>
              event.kind === "submission_intent" &&
              event.actionId.startsWith(SLASH),
          ),
        ).toBe(false);
        expect(await defenderNodes(journey)).toStrictEqual([]);
        // The other watcher's slash paid the reward to our funding key all
        // the same.
        const rewards = (
          await scene.h.emulator.getUtxos(
            credentialToAddress(network, {
              type: "Key",
              hash: scene.h.proverSigner.paymentKeyHash,
            }),
          )
        ).filter(({ txHash }) => txHash === foreign[0]);
        expect(rewards.map(({ assets }) => assets)).toStrictEqual([
          {
            lovelace: fraudSlashEconomicsFromDeploymentManifest(
              journey.removal.removal.deploymentInfo,
            ).fraudProverRewardLovelace,
          },
        ]);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "raises the operator's alarm, naming the defender's bond unlock, when the slash does not build, once the thread's close has landed and settled the case",
    async () => {
      // The watcher's removal names a deployment whose active-operators
      // reference script was never published.
      const journey = await openTwoDefendedFundedCase({
        rewriteRemoval: (recorded) => {
          const info = recorded.deploymentInfo as Readonly<{
            contracts: Readonly<Record<string, object>>;
          }>;
          return {
            ...recorded,
            deploymentInfo: {
              ...info,
              contracts: {
                ...info.contracts,
                activeOperatorsSpend: {
                  ...info.contracts.activeOperatorsSpend,
                  refScriptUTxO: undefined,
                },
              },
            },
          };
        },
      });
      const { dispute } = journey;
      try {
        const [node] = await defenderNodes(journey);
        const { result, alarms, history } = await playToDefenderWon(
          journey,
          undefined,
          VALIDATION_TRACE_MOVE_UNBUILT,
        );
        // The thread's close landed, its receipt minted, and the case is
        // settled for us: the unbuilt slash held none of it back.
        expect(
          confirmed(result).map(({ stage }) => stage),
          history.join("\n"),
        ).toContain("close_thread");
        expect(
          await journey.scene.h.proverLucid.utxosAtWithUnit(
            journey.scene.chain.award.spendingScriptAddress,
            receiptUnit(journey),
          ),
        ).toHaveLength(1);
        expect(
          Data.from(
            (
              await liveCase({
                scene: journey.scene,
                caseUnit: caseUnitOf(journey),
              })
            ).datum!,
            SDK.Case,
          ),
        ).toMatchObject({ open_threads: 0n, defender_won: true });
        const element = Data.from(node!.datum!, SDK.LinkedListDatum).data;
        if (!("Node" in element)) throw new Error("the defender has no node");
        const unlock = Data.from(
          Data.to(element.Node.data),
          SDK.ActiveOperatorDatum,
        ).bond_unlock_time;
        expect(unlock).not.toBeNull();
        // The run is held under the alarm, which names the slash.
        const slash = [
          ...alarms,
          ...("reason" in result ? [result.reason] : []),
        ].filter(
          (reason) =>
            reason.includes("the slash_lost_defense move") ||
            reason.includes(" slashing defender "),
        );
        expect(slash.length, history.join("\n")).toBeGreaterThan(0);
        // The watcher holds the run under the alarm state.
        const state = validationTraceRunState(result);
        expect(
          "namedState" in state ? state.namedState : undefined,
          history.join("\n"),
        ).toStrictEqual({ name: VALIDATION_TRACE_MOVE_UNBUILT, alarm: true });
        for (const reason of slash) {
          expect(reason.startsWith(VALIDATION_TRACE_MOVE_UNBUILT)).toBe(true);
          expect(reason).toContain(
            ` ${unlock!.toString()}, the defender's bond unlock`,
          );
        }
        // The defender keeps its node until a slash lands.
        expect(await defenderNodes(journey)).toHaveLength(1);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
