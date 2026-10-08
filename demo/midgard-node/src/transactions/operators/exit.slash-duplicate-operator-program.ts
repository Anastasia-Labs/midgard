import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type IntentJournal,
  journaledIntent,
} from "../../services/intent-journal.js";
import { handleSignSubmit } from "../utils.js";
import {
  exitValidityWindow,
  type OperatorEconomics,
  type OperatorExitError,
  OperatorExitRefusal,
  type PlannedSnapshot,
  plannedSnapshotProgram,
  resolveOperatorScriptRefsProgram,
} from "./exit.resolve-operator-script-refs-program.js";
import { type DuplicateSlashSubmission } from "./exit.retire-operator-program.js";
import {
  collateralForExactFee,
  requireOperatorFundingProgram,
} from "./funding-preflight.js";

/**
 * Picks the registered node to remove and the membership that proves it a
 * duplicate. An active or retired membership is the proof when one exists;
 * otherwise two registered nodes prove each other and the later registration
 * is the one removed. A single registration yields `null`: a node cannot prove
 * its own duplication, because the ledger requires a transaction's reference
 * inputs to be disjoint from its inputs, so there is nothing to slash.
 */
export const selectDuplicateRegistration = (
  view: SDK.OperatorDirectoryView &
    Pick<SDK.OperatorDirectorySnapshot, "hubOracle">,
  operatorKeyHash: string,
): {
  readonly removed: SDK.NodeWithDatum;
  readonly proof: SDK.DuplicateProof;
} | null => {
  const occupancies = SDK.findOperatorDirectoryOccupancies(
    view,
    operatorKeyHash,
  );
  const registered = occupancies.filter(({ kind }) => kind === "registered");
  if (registered.length === 0) {
    return null;
  }
  const active = occupancies.find(({ kind }) => kind === "active");
  const retired = occupancies.find(({ kind }) => kind === "retired");
  const byKeyDescending = [...registered].sort((left, right) => {
    const l = SDK.nodeKeyHex(left.node.datum.key) ?? "";
    const r = SDK.nodeKeyHex(right.node.datum.key) ?? "";
    return l < r ? 1 : l > r ? -1 : 0;
  });
  const removed = byKeyDescending[0]!.node;
  if (active !== undefined) {
    return {
      removed,
      proof: {
        kind: "active",
        node: active.node,
        hubOracleRefInput: view.hubOracle.utxo,
      },
    };
  }
  if (retired !== undefined) {
    return { removed, proof: { kind: "retired", node: retired.node } };
  }
  if (byKeyDescending.length >= 2) {
    return {
      removed,
      proof: { kind: "registered", node: byKeyDescending[1]!.node },
    };
  }
  return null;
};

/**
 * Removes a duplicate registered node. Anyone may submit; the slashing
 * penalty is paid from the removed bond as the transaction fee and the rest of
 * that bond is left to the submitter as change.
 */
export const slashDuplicateOperatorProgram = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  referenceScriptsAddress: string,
  input: {
    readonly operatorKeyHash: string;
    readonly economics: OperatorEconomics;
  } & PlannedSnapshot,
  options: { readonly label?: string } = {},
): Effect.Effect<DuplicateSlashSubmission, OperatorExitError, IntentJournal> =>
  Effect.gen(function* () {
    const label = options.label ?? "slash-duplicate-operator";
    const { snapshot, intentPlan } = yield* plannedSnapshotProgram(
      lucid,
      contracts,
      input,
    );
    const selection = selectDuplicateRegistration(
      snapshot,
      input.operatorKeyHash,
    );
    if (selection === null) {
      const occupancies = SDK.findOperatorDirectoryOccupancies(
        snapshot,
        input.operatorKeyHash,
      ).map(({ kind }) => kind);
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Operator ${input.operatorKeyHash} has no duplicate registration to slash (memberships: ${occupancies.length === 0 ? "none" : occupancies.join(", ")})`,
          input.operatorKeyHash,
          { memberships: occupancies.join(",") || null },
        ),
      );
    }
    const removedKey = SDK.nodeKeyHex(selection.removed.datum.key);
    if (removedKey === null) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          "Selected a root node as the duplicate registration; refusing",
          input.operatorKeyHash,
        ),
      );
    }
    const registeredAnchor = SDK.findAnchorNodeForKey(
      snapshot.registered,
      removedKey,
    );
    if (registeredAnchor === undefined) {
      return yield* Effect.fail(
        new OperatorExitRefusal(
          `Found no registered-operators element linking to node ${removedKey}`,
          input.operatorKeyHash,
        ),
      );
    }

    // The slashing penalty leaves as the fee, so the ledger wants the
    // collateral percentage of the whole penalty in the submitter's wallet.
    yield* requireOperatorFundingProgram(lucid, {
      label,
      lockedLovelace: 0n,
      collateralLovelace: collateralForExactFee(
        lucid,
        input.economics.slashingPenaltyLovelace,
      ),
    });
    const scriptRefs = yield* resolveOperatorScriptRefsProgram(
      lucid,
      contracts,
      referenceScriptsAddress,
      ["registered-operators"],
    );
    const { validFrom, validTo } = exitValidityWindow(lucid);
    const { tx } = yield* SDK.buildUnsignedSlashDuplicateOperatorTxProgram({
      lucid,
      contracts,
      operatorKeyHash: input.operatorKeyHash,
      registeredOperatorScriptRefs: scriptRefs.family("registered-operators"),
      duplicateRegisteredNode: selection.removed,
      registeredAnchor,
      duplicateRegisteredNodeUnit: toUnit(
        contracts.registeredOperators.policyId,
        SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + removedKey,
      ),
      duplicateProof: selection.proof,
      slashingPenaltyLovelace: input.economics.slashingPenaltyLovelace,
      validFrom,
      validTo,
    });
    const feeLovelace = tx.toTransaction().body().fee();
    const txHash = yield* handleSignSubmit(
      lucid,
      tx,
      journaledIntent("exit", `exit:slash_duplicate:${removedKey}`, intentPlan),
      { label },
    );
    return {
      txHash,
      operatorKeyHash: input.operatorKeyHash,
      removedRegisteredNodeKey: removedKey,
      proofKind: selection.proof.kind,
      feeLovelace,
      submitterRewardLovelace:
        (selection.removed.utxo.assets["lovelace"] ?? 0n) - feeLovelace,
    };
  });
