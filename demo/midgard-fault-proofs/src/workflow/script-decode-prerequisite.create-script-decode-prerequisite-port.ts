/**
 * The port that records a Plutus item's script decode as journaled side
 * actions of a family workflow. `inspect` reads the chain (through the
 * provider: liveness only, as the validator checks every step) and the
 * journal, and names the one action the record needs next; `capture` builds
 * and signs it from the reserved funding; `reconcile` settles a submitted one
 * from its signed bytes on authenticated L1, or else from its exact output.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { scriptDecodeRunTokenName } from "../script-decode/data.js";
import { plutusItemScript } from "../script-decode/item-record.js";
import {
  findScriptDecodeRecord,
  fitScriptDecodeWalk,
  refreshScriptDecodeLedgerParameters,
  SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
  scriptDecodeLedgerLimits,
} from "../script-decode/record.js";
import { findStagedScriptDecodeChunks } from "../script-decode/resume.js";
import {
  buildScriptDecodeBindTx,
  buildScriptDecodeFinishTx,
  buildScriptDecodeStageTx,
  buildScriptDecodeWalk,
  type ScriptDecodeDeployment,
  type ScriptDecodeSignedTx,
  scriptDecodeStagingDatums,
} from "../script-decode/submit.js";
import { WorkflowActionChangedError } from "./action-changed.js";
import { TX_HASH } from "./field-carriage-prerequisite.field-carriage-prerequisite-port.js";
import type { JournalJsonObject } from "./journal.js";
import type { FraudProofWorkflowReconcileResult } from "./orchestrator.js";
import type { FraudProofAuthenticatedPublicationObserver } from "./raw-l1-publication-observation.js";
import {
  confirmedScriptDecodeOutputs,
  parseScriptDecodeAction,
  parseScriptDecodeRecovery,
  SCRIPT_DECODE_PREREQUISITE,
  scriptDecodeAction,
  scriptDecodeAttemptsExhausted,
  scriptDecodeDatumDigest,
  scriptDecodeRecovery,
  scriptDecodeRejectedAttempts,
} from "./script-decode-prerequisite.actions.js";
import { scriptDecodeReferenceCandidates } from "./script-decode-prerequisite.deployment.js";
import {
  SCRIPT_DECODE_RECORD_HELD,
  type ScriptDecodeItemForAction,
  type ScriptDecodePrerequisitePort,
  ScriptDecodeRecordHeldError,
  ScriptDecodeRecordUnderfundedError,
} from "./script-decode-prerequisite.port.js";
import {
  rememberScriptDecodeCursor,
  scriptDecodeCursorAtRun,
  scriptDecodeFuelGuess,
} from "./script-decode-prerequisite.walk-cursor.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";
import {
  type LocallyEvaluatedTransaction,
  workflowReferenceScriptsUsedByTransaction,
} from "./transaction-boundary.js";

/**
 * How long a confirmed output the provider does not show is taken for
 * provider lag rather than spent.
 */
export const SCRIPT_DECODE_PROVIDER_GRACE_MS = 10 * 60_000;

/** How long a signed script-decode transaction stays valid. */
export const SCRIPT_DECODE_VALIDITY_MS = 15 * 60_000;

/** Our newest runs looked up at once: a rollback revives at most these. */
const RUN_LOOKBACK = 16;

const outRefOf = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const parseOutRef = (outRef: string) => {
  const [txHash, index] = outRef.split("#");
  return { txHash: txHash!, outputIndex: Number(index) };
};

const UNDERFUNDED = /EMPTY_UTXO|does not have enough funds/u;

const describe = (value: unknown): string => {
  if (value instanceof Error) return value.message;
  if (typeof value === "string") return value;
  try {
    return JSON.stringify(value) ?? "";
  } catch {
    return "";
  }
};

/** True when a build failed for want of wallet funds, at any cause depth. */
const underfunded = (cause: unknown): boolean => {
  let at = cause;
  for (let depth = 0; at !== undefined && at !== null && depth < 8; depth++) {
    if (UNDERFUNDED.test(describe(at))) return true;
    at = at instanceof Error ? at.cause : undefined;
  }
  return false;
};

/** Records each Plutus item `itemForAction` names before its proof step. */
export const createScriptDecodePrerequisitePort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  signer,
  deployment,
  itemForAction,
  publications,
  now = Date.now,
  maxAttempts = SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
  refreshLedgerParameters = refreshScriptDecodeLedgerParameters(lucid),
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: () => ScriptDecodeDeployment;
  readonly itemForAction: ScriptDecodeItemForAction;
  readonly publications: FraudProofAuthenticatedPublicationObserver;
  readonly now?: () => number;
  /** Submissions of one action the ledger may reject before it is held. */
  readonly maxAttempts?: number;
  readonly refreshLedgerParameters?: () => Promise<void>;
}): ScriptDecodePrerequisitePort<Category> => {
  const scriptOf = async (input: {
    readonly action: Parameters<ScriptDecodeItemForAction>[0]["action"];
    readonly artifact: JournalJsonObject;
  }) => {
    const item = await itemForAction(input);
    return item === null ? undefined : plutusItemScript(item);
  };
  const recent = (confirmedAt: string): boolean =>
    now() - Date.parse(confirmedAt) < SCRIPT_DECODE_PROVIDER_GRACE_MS;
  const runUnit = (scriptHashHex: string) =>
    `${deployment().validator.policyId}${scriptDecodeRunTokenName(scriptHashHex)}`;
  const runAt = async (outRef: string): Promise<UTxO | undefined> =>
    (await lucid.utxosByOutRef([parseOutRef(outRef)]))[0];

  const port: ScriptDecodePrerequisitePort<Category> = {
    portVersion: SCRIPT_DECODE_PREREQUISITE,
    category,
    inspect: async ({ baseAction, artifact, entries }) => {
      const script = await scriptOf({ action: baseAction, artifact });
      if (script === undefined) return { kind: "not_required" };
      const { scriptHashHex, scriptBytes, language } = script;
      if (
        (await findScriptDecodeRecord({
          lucid,
          deployment: deployment(),
          scriptHashHex,
        })) !== undefined
      ) {
        return { kind: "satisfied" };
      }
      const required = (
        stage: Parameters<typeof scriptDecodeAction>[0]["stage"],
        fields: Parameters<typeof scriptDecodeAction>[0]["fields"],
      ) => {
        const action = scriptDecodeAction({
          category,
          baseAction,
          scriptHashHex,
          stage,
          fields,
        } as Parameters<typeof scriptDecodeAction>[0]);
        return scriptDecodeAttemptsExhausted(
          entries,
          action.actionId,
          maxAttempts,
        )
          ? ({
              kind: "held",
              action,
              reason: `${SCRIPT_DECODE_RECORD_HELD}: ${action.actionId} was rejected on all ${maxAttempts.toString()} attempts`,
            } as const)
          : ({ kind: "required", action } as const);
      };
      const lagging = (outRef: string) =>
        ({
          kind: "pending",
          reason: `script decode output ${outRef} is not yet visible to the provider`,
        }) as const;
      const confirmed = (
        stages: Parameters<typeof confirmedScriptDecodeOutputs>[0]["stages"],
      ) =>
        confirmedScriptDecodeOutputs({
          entries,
          category,
          scriptHashHex,
          stages,
        });

      const datums = scriptDecodeStagingDatums(scriptBytes);
      const chunks = await findStagedScriptDecodeChunks({
        lucid,
        scriptBytes,
        address: signer.address,
      });
      const missing = chunks.findIndex((chunk) => chunk === undefined);
      if (missing >= 0) {
        const replaced = confirmed(["script_decode_stage"])
          .filter(({ action }) => action.chunkIndex === missing)
          .at(-1);
        if (replaced !== undefined && recent(replaced.confirmedAt)) {
          return lagging(replaced.recovery.outRef);
        }
        return required("script_decode_stage", {
          chunkIndex: missing,
          datumCborSha256: scriptDecodeDatumDigest(datums[missing]!),
          replacesOutRef: replaced?.recovery.outRef ?? null,
        });
      }

      const runs = confirmed(["script_decode_bind", "script_decode_walk"])
        .slice(-RUN_LOOKBACK)
        .reverse();
      const visible = new Map(
        (
          await lucid.utxosByOutRef(
            runs.map(({ recovery }) => parseOutRef(recovery.outRef)),
          )
        ).map((utxo) => [outRefOf(utxo), utxo]),
      );
      const newest = runs[0];
      const current = runs
        .map(({ recovery }) => visible.get(recovery.outRef))
        .find((utxo) => utxo !== undefined);
      if (
        current === undefined ||
        outRefOf(current) !== newest?.recovery.outRef
      ) {
        if (newest !== undefined && recent(newest.confirmedAt)) {
          return lagging(newest.recovery.outRef);
        }
      }
      if (current === undefined) {
        return required("script_decode_bind", {
          previousRunOutRef: newest?.recovery.outRef ?? null,
        });
      }
      const cursor = scriptDecodeCursorAtRun({
        language,
        scriptBytes,
        scriptHashHex,
        runUtxo: current,
      });
      const runOutRef = outRefOf(current);
      if (cursor === undefined) {
        return required("script_decode_bind", { previousRunOutRef: runOutRef });
      }
      return scriptDecodeWalkRecord(cursor) === undefined
        ? required("script_decode_walk", { runOutRef })
        : required("script_decode_finish", { runOutRef });
    },
    capture: async ({ action, artifact, entries }) => {
      const parsed = parseScriptDecodeAction(action, category);
      const script = await scriptOf({ action: parsed.baseAction, artifact });
      if (script?.scriptHashHex !== parsed.scriptHashHex) {
        throw new WorkflowActionChangedError(
          `${category} script decode item changed`,
        );
      }
      const { scriptHashHex, scriptBytes, language } = script;
      const bound = deployment();
      const changed = (what: string) =>
        new WorkflowActionChangedError(
          `${category} script decode ${what} is no longer on chain`,
        );
      // A rejected attempt is refitted against freshly read ledger limits.
      if (scriptDecodeRejectedAttempts(entries, action.actionId) > 0) {
        await refreshLedgerParameters();
      }
      signer.selectWallet(lucid);
      const validTo =
        lucid.slotToUnixTime(lucid.currentSlot()) + SCRIPT_DECODE_VALIDITY_MS;
      const staging = async () => {
        const chunks = await findStagedScriptDecodeChunks({
          lucid,
          scriptBytes,
          address: signer.address,
        });
        if (chunks.some((chunk) => chunk === undefined)) {
          throw changed("staging");
        }
        return chunks as UTxO[];
      };
      const run = async () => {
        const runUtxo = await runAt(parsed.runOutRef!);
        if (runUtxo === undefined) throw changed("run");
        const cursor = scriptDecodeCursorAtRun({
          language,
          scriptBytes,
          scriptHashHex,
          runUtxo,
        });
        if (cursor === undefined) throw changed("honest run");
        return { runUtxo, cursor };
      };
      let built: ScriptDecodeSignedTx;
      let unit: string | null = runUnit(scriptHashHex);
      try {
        switch (parsed.stage) {
          case "script_decode_stage": {
            const datum =
              scriptDecodeStagingDatums(scriptBytes)[parsed.chunkIndex!];
            if (
              datum === undefined ||
              scriptDecodeDatumDigest(datum) !== parsed.datumCborSha256
            ) {
              throw changed("staging chunk");
            }
            built = await buildScriptDecodeStageTx({
              lucid,
              datum,
              address: signer.address,
              validTo,
            });
            unit = null;
            break;
          }
          case "script_decode_bind":
            built = await buildScriptDecodeBindTx({
              lucid,
              deployment: bound,
              language,
              scriptHashHex,
              initialRun: startScriptDecodeWalk(language, scriptBytes).run,
              staging: await staging(),
              validTo,
            });
            break;
          case "script_decode_walk": {
            const { runUtxo, cursor } = await run();
            const chunks = await staging();
            const fitted = await fitScriptDecodeWalk(
              cursor,
              scriptDecodeFuelGuess(scriptHashHex),
              scriptDecodeLedgerLimits(lucid),
              (walk) =>
                buildScriptDecodeWalk({
                  lucid,
                  deployment: bound,
                  scriptHashHex,
                  runUtxo,
                  staging: chunks,
                  walk,
                  validTo,
                }),
            );
            rememberScriptDecodeCursor(scriptHashHex, fitted.next, fitted.fuel);
            built = fitted.built.tx;
            break;
          }
          case "script_decode_finish": {
            const { runUtxo, cursor } = await run();
            const record = scriptDecodeWalkRecord(cursor);
            if (record === undefined) throw changed("finished run");
            built = await buildScriptDecodeFinishTx({
              lucid,
              deployment: bound,
              scriptHashHex,
              runUtxo,
              record,
              validTo,
            });
            unit = `${bound.validator.policyId}${scriptHashHex}`;
            break;
          }
        }
      } catch (cause) {
        if (cause instanceof WorkflowActionChangedError) throw cause;
        if (underfunded(cause)) {
          throw new ScriptDecodeRecordUnderfundedError(parsed.stage, cause);
        }
        throw new ScriptDecodeRecordHeldError(parsed.stage, cause);
      }
      const transaction: LocallyEvaluatedTransaction = Object.freeze({
        txHash: built.signed.toHash().toLowerCase(),
        signed: built.signed,
        referenceScripts: Object.freeze(
          workflowReferenceScriptsUsedByTransaction({
            signed: built.signed,
            candidates: scriptDecodeReferenceCandidates(bound),
          }),
        ),
      });
      return {
        transaction,
        durableRecovery: scriptDecodeRecovery({
          stage: parsed.stage,
          transaction,
          address: built.address,
          matches: built.matches,
          unit,
        }),
      };
    },
    reconcile: async ({
      action,
      txHash,
      durableRecovery,
      signedTransactionCborHex,
      authorizeResubmission,
    }): Promise<FraudProofWorkflowReconcileResult> => {
      if (txHash === undefined || !TX_HASH.test(txHash)) {
        return {
          kind: "conflict",
          reason: `${category} script decode action omitted its exact transaction hash`,
        };
      }
      let recovery: ReturnType<typeof parseScriptDecodeRecovery>;
      try {
        recovery = parseScriptDecodeRecovery({
          value: durableRecovery,
          stage: parseScriptDecodeAction(action, category).stage,
          txHash,
        });
      } catch (cause) {
        return { kind: "conflict", reason: String(cause) };
      }
      if (
        signedTransactionCborHex !== undefined &&
        publications.observeSignedTransaction !== undefined
      ) {
        const reconciled = await reconcileSignedWorkflowTransaction({
          transactionHash: txHash,
          signedTransactionCborHex,
          observe: publications.observeSignedTransaction,
          rebroadcast: publications.rebroadcastSignedTransaction,
          authorizeResubmission,
          reportInclusion: true,
        });
        // Included deeper than automatic recovery reaches is still included.
        return reconciled.kind === "pending" &&
          reconciled.retirement?.reason === "included"
          ? { kind: "confirmed", txHash }
          : reconciled;
      }
      // Without the signed bytes (a funding confirmation journaled before its
      // reconciliation), the exact output the intent journaled settles it. A
      // record transaction touches no family history unit, so the family's
      // authenticated L1 never shows it; the provider does, for liveness
      // only, as the validator checks every record step it continues from.
      const output = await runAt(recovery.outRef);
      return output !== undefined &&
        output.address === recovery.address &&
        output.datum !== undefined &&
        output.datum !== null &&
        output.scriptRef == null &&
        scriptDecodeDatumDigest(output.datum) === recovery.datumCborSha256 &&
        (recovery.unit === null || output.assets[recovery.unit] === 1n)
        ? { kind: "confirmed", txHash }
        : { kind: "pending", txHash };
    },
  };
  return Object.freeze(port);
};
