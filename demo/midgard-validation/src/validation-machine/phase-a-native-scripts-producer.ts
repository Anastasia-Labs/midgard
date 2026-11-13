/**
 * The PhaseANativeScripts producer: each script witness's header, and each
 * native script scanned whole. It hands the preconditions phase whether any
 * witness is not native.
 */
import { MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX } from "./canonical-field-item.js";
import {
  readValidationMachineVersionedScriptHeader,
  type ValidationMachineVersionedScriptHeader,
} from "./native-script-frame.js";
import {
  type NativeScanPrepared,
  phaseANativeScriptsScanWitnessCbor,
  scanNativeScriptItem,
} from "./native-script-scan.js";
import {
  preparedFieldPreimage,
  type PreparedValidationTransaction,
} from "./prepared-validation-transaction.js";
import type { PhaseANativeScriptsScanControl } from "./trace-builder-types.js";
import {
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
} from "./trace-walk.js";
import type { ValidationMachineWorkWitness } from "./types.js";

const openControl = (
  scriptCount: number,
  scriptSeen: number,
  containsNonNativeScript: 0 | 1,
): PhaseANativeScriptsScanControl => ({
  stage: 0,
  scriptCount,
  scriptSeen,
  containsNonNativeScript,
  itemLength: 0,
  itemCommitment: Buffer.alloc(0),
  cursor: 0,
  stackRoot: Buffer.alloc(0),
  stackDepth: 0,
  nodeCount: 0,
  result: -1,
  purposeKind: -1n,
  purposeIndex: -1n,
});

export const producePhaseANativeScripts = (
  prepared: NativeScanPrepared &
    Pick<
      PreparedValidationTransaction,
      "fieldPreimages" | "scriptWitnessesCollection"
    >,
  { push }: Pick<TraceWalk, "push">,
): {
  readonly outcome: PhaseOutcome;
  readonly containsNonNativeScript: 0 | 1;
} => {
  const { items } = prepared.scriptWitnessesCollection;
  let control = openControl(items.length === 0 ? 0 : -1, 0, 0);
  const done = (outcome: PhaseOutcome) => ({
    outcome,
    containsNonNativeScript: control.containsNonNativeScript,
  });
  const pushWitness = (
    auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
  ): void => {
    push(
      "phaseANativeScripts",
      phaseANativeScriptsScanWitnessCbor(prepared, control),
      auxiliary,
    );
  };

  if (control.scriptCount === 0) {
    pushWitness();
    return done(phaseContinues);
  }
  for (const item of items) {
    const scriptCount =
      control.scriptCount === -1 ? items.length : control.scriptCount;
    pushWitness({
      kind: "transactionFieldChunk",
      fieldIndex: MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
      itemIndex: item.itemIndex,
      fieldPreimage: preparedFieldPreimage(
        prepared,
        MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX,
      ),
    });

    let header: ValidationMachineVersionedScriptHeader;
    try {
      header = readValidationMachineVersionedScriptHeader(item.bytes);
    } catch {
      return done(
        phaseStops(
          {
            WitnessScriptHeaderMalformed: {
              script_index: BigInt(control.scriptSeen),
            },
          },
          `bounded native-script scan stopped at stage=${control.stage},cursor=${control.cursor}`,
        ),
      );
    }

    if (header.languageTag !== 0) {
      control = openControl(scriptCount, control.scriptSeen + 1, 1);
      continue;
    }

    const scan = scanNativeScriptItem(prepared, push, item, {
      ...control,
      stage: 1,
      scriptCount,
      itemLength: item.bytes.length,
      itemCommitment: item.commitment,
      cursor: header.payloadOffset,
    });
    control = scan.control;
    if (scan.stop !== null) return done(scan.stop);
    control = openControl(
      scriptCount,
      control.scriptSeen + 1,
      control.containsNonNativeScript,
    );
  }
  return done(phaseContinues);
};
