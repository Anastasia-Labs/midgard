/** The funded two-defender case whose silent operator loses its thread, as
 * the lost-defense slash files play it: the defender's node, its receipt,
 * the run to defender-won, and another watcher's slash of the defender. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, getAddressDetails, type UTxO } from "@lucid-evolution/lucid";

import { confirmedValidationTraceStages } from "../../src/validation-trace-dispute/actions.js";
import { captureValidationTraceLostDefenseSlash } from "../../src/validation-trace-dispute/lost-defense-slash.js";
import { VALIDATION_TRACE_DEFENDER_WON } from "../../src/validation-trace-dispute/threads.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { anotherParty } from "./validation-trace-dispute-foreign-verify.js";
import {
  type FundedMoves,
  type SecondDefense,
  successorWinsItsThread,
} from "./validation-trace-dispute-funded-moves.js";
import type { AdapterWrap } from "./validation-trace-dispute-funded-runner.js";
import { entriesOf } from "./validation-trace-dispute-funded-verify-source.js";

export type TwoDefended = FundedMoves & Readonly<{ second: SecondDefense }>;

export const SLASH = "validation-trace-slash-lost-defense:";

export const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The defender's operator node, in whichever operator set holds it. */
export const defenderNodes = async ({ scene, defender }: FundedMoves) => {
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
export const receiptUnit = ({ scene, unit }: FundedMoves) =>
  scene.chain.award.policyId + unit.slice(56);

/** The bond unlock an active operator's node names, if any. */
export const nodeBondUnlock = (node: UTxO): bigint | null => {
  const element = Data.from(node.datum!, SDK.LinkedListDatum).data;
  if (!("Node" in element)) throw new Error("the defender has no node");
  return Data.from(Data.to(element.Node.data), SDK.ActiveOperatorDatum)
    .bond_unlock_time;
};

/** A removal whose deployment never published the active-operators
 * reference script: the slash does not build. */
export const unpublishedActiveOperators = <T>(recorded: T): T => {
  const info = (recorded as { deploymentInfo: unknown })
    .deploymentInfo as Readonly<{
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
};

/**
 * The successor wins its thread while the operator stays silent past its
 * deadline, and the watcher turns, with `wrap`, until the run is pending
 * for `until` (defender-won), every alarm each turn raised kept.
 */
export const playToDefenderWon = async (
  journey: TwoDefended,
  wrap?: AdapterWrap,
  until = VALIDATION_TRACE_DEFENDER_WON,
) => {
  await successorWinsItsThread(journey);
  return await turnCollecting(journey, wrap, until);
};

/** The watcher turns, with `wrap`, until the run is pending for `until`,
 * every alarm each turn raised kept. */
export const turnCollecting = async (
  { dispute }: TwoDefended,
  wrap?: AdapterWrap,
  until = VALIDATION_TRACE_DEFENDER_WON,
) => {
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
export const confirmed = (result: FraudProofWorkflowRunResult) =>
  confirmedValidationTraceStages(entriesOf(result)).map(
    ({ action, txHash }) => ({ stage: action.stage, txHash }),
  );

/** Another watcher's slash of the defender by the receipt at
 * `receiptOutRef`, landed; returns its hash. */
export const landForeignSlash = async (
  journey: TwoDefended,
  receiptOutRef: string,
): Promise<string> => {
  const { scene, dispute } = journey;
  const lucid = await anotherParty(scene);
  const address = await lucid.wallet().address();
  const { transaction } = await captureValidationTraceLostDefenseSlash({
    lucid,
    signer: {
      source: "direct-seed-phrase",
      address,
      paymentKeyHash: getAddressDetails(address).paymentCredential!.hash,
      selectWallet: () => undefined,
    },
    deployment: dispute.deployment,
    removal: journey.removal.removal,
    headerHash: scene.failing.evidence.headerHash,
    receiptOutRef,
  });
  const txHash = await transaction.signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
