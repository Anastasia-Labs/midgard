/**
 * The port that records a Plutus item's script decode as side actions of a
 * family workflow. `inspect` reads the chain (through the provider: liveness
 * only, as the validator checks every step) and names the one action the
 * record needs next; `capture` builds and signs it from the prover wallet.
 * The workflow follows each submitted one by its signed bytes.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import {
  scriptDecodeWalkRecord,
  startScriptDecodeWalk,
} from "@al-ft/midgard-validation/script-decode.step-walk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { plutusItemScript } from "../script-decode/item-record.js";
import {
  findScriptDecodeRecord,
  fitScriptDecodeWalk,
  scriptDecodeLedgerLimits,
} from "../script-decode/record.js";
import { resumeScriptDecodeRun } from "../script-decode/resume.js";
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
import type { JournalJsonObject } from "./journal.js";
import {
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "./proof-materials-view.js";
import {
  parseScriptDecodeAction,
  SCRIPT_DECODE_PREREQUISITE,
  scriptDecodeAction,
  scriptDecodeDatumDigest,
} from "./script-decode-prerequisite.actions.js";
import { scriptDecodeReferenceCandidates } from "./script-decode-prerequisite.deployment.js";
import {
  type ScriptDecodeItemForAction,
  type ScriptDecodePrerequisitePort,
  ScriptDecodeRecordHeldError,
  ScriptDecodeRecordUnderfundedError,
} from "./script-decode-prerequisite.port.js";
import { scriptDecodeStagedChunks } from "./script-decode-prerequisite.staged-chunks.js";
import {
  rememberScriptDecodeCursor,
  scriptDecodeCursorAtRun,
  scriptDecodeFuelGuess,
} from "./script-decode-prerequisite.walk-cursor.js";
import {
  type LocallyEvaluatedTransaction,
  workflowReferenceScriptsUsedByTransaction,
} from "./transaction-boundary.js";
import { isWorkflowFundingShortfall } from "./workflow-funding-shortfall.js";

/** How long a signed script-decode transaction stays valid. */
export const SCRIPT_DECODE_VALIDITY_MS = 15 * 60_000;

const outRefOf = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const parseOutRef = (outRef: string) => {
  const [txHash, index] = outRef.split("#");
  return { txHash: txHash!, outputIndex: Number(index) };
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
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: () => ScriptDecodeDeployment;
  readonly itemForAction: ScriptDecodeItemForAction;
}): ScriptDecodePrerequisitePort<Category> => {
  const scriptOf = async (input: {
    readonly action: Parameters<ScriptDecodeItemForAction>[0]["action"];
    readonly artifact: JournalJsonObject;
  }) => {
    const item = await itemForAction(input);
    return item === null ? undefined : plutusItemScript(item);
  };
  const runAt = async (outRef: string): Promise<UTxO | undefined> =>
    (await lucid.utxosByOutRef([parseOutRef(outRef)]))[0];
  const stagedChunks = (input: {
    readonly scriptBytes: Uint8Array;
    readonly holder: ProofMaterialsHolder;
  }) =>
    scriptDecodeStagedChunks({
      scriptBytes: input.scriptBytes,
      lucid,
      address: signer.proofMaterialsAddress,
      materials: proofMaterialsAccess({ signer, lucid, holder: input.holder }),
    });

  const port: ScriptDecodePrerequisitePort<Category> = {
    portVersion: SCRIPT_DECODE_PREREQUISITE,
    category,
    need: async ({ holder, baseAction, artifact }) => {
      const script = await scriptOf({ action: baseAction, artifact });
      if (script === undefined) return;
      proofMaterialsAccess({ signer, lucid, holder }).view.need(
        holder,
        scriptDecodeStagingDatums(script.scriptBytes),
      );
    },
    inspect: async ({ baseAction, artifact, holder }) => {
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
      ) =>
        ({
          kind: "required",
          action: scriptDecodeAction({
            category,
            baseAction,
            scriptHashHex,
            stage,
            fields,
          } as Parameters<typeof scriptDecodeAction>[0]),
        }) as const;
      const staged = await stagedChunks({ scriptBytes, holder });
      if (staged.kind === "publish") {
        return required("script_decode_stage", {
          chunkIndex: staged.ordinal,
          datumCborSha256: scriptDecodeDatumDigest(
            scriptDecodeStagingDatums(scriptBytes)[staged.ordinal]!,
          ),
        });
      }

      // The open run on chain furthest along that the honest walk writes,
      // whoever bound it: bind is permissionless and every honest run
      // finishes into the one record.
      const resumed = await resumeScriptDecodeRun({
        lucid,
        deployment: deployment(),
        language,
        scriptBytes,
        scriptHashHex,
      });
      // No record and no run, both read at the validator's address.
      if (resumed === undefined) return required("script_decode_bind", {});
      const runOutRef = outRefOf(resumed.runUtxo);
      return scriptDecodeWalkRecord(resumed.cursor) === undefined
        ? required("script_decode_walk", { runOutRef })
        : required("script_decode_finish", { runOutRef });
    },
    capture: async ({ action, artifact, holder }) => {
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
      signer.selectWallet(lucid);
      const validTo =
        lucid.slotToUnixTime(lucid.currentSlot()) + SCRIPT_DECODE_VALIDITY_MS;
      const staging = async () => {
        const staged = await stagedChunks({ scriptBytes, holder });
        if (staged.kind !== "ready") throw changed("staging");
        return [...staged.utxos];
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
              address: signer.proofMaterialsAddress,
              validTo,
            });
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
            break;
          }
        }
      } catch (cause) {
        if (cause instanceof WorkflowActionChangedError) throw cause;
        if (isWorkflowFundingShortfall(cause)) {
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
      return { transaction };
    },
  };
  return Object.freeze(port);
};
