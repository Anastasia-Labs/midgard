import {
  type MidgardNativeScriptDecodingDirection,
  MidgardNativeScriptDecodingDirections,
} from "@al-ft/midgard-core";
import {
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION,
} from "@al-ft/midgard-sdk";

import { type NativeScriptDecodingFinding } from "./finding.js";
import {
  type DriveState,
  step03TxCount,
} from "./prover.locate-native-script-decoding-thread.js";
import { type NativeScriptDecodingScanPlan } from "./scan-plan.js";

export const remainingTxCount = (
  cursor: DriveState,
  finding: NativeScriptDecodingFinding,
  plan: NativeScriptDecodingScanPlan | null,
): number => {
  const step03 = step03TxCount(finding, plan);
  switch (cursor.at) {
    case "init":
      return 3 + step03 + 1;
    case "step01":
      return 2 + step03 + 1;
    case "step02":
      return 1 + step03 + 1;
    case "openSubject":
      return step03 + 1;
    case "bindDescriptor":
      return step03;
    case "advanceOrClose": {
      const explicitClose =
        finding.direction ===
        NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE
          ? 1
          : 0;
      return (
        (plan?.segments.length ?? 0) - cursor.segmentIndex + explicitClose + 1
      );
    }
    case "close":
      return 2;
    case "step04":
      return 1;
  }
};

export const coreDirectionOf = (
  finding: NativeScriptDecodingFinding,
): MidgardNativeScriptDecodingDirection =>
  finding.direction === NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION
    ? MidgardNativeScriptDecodingDirections.WrongfulRejection
    : MidgardNativeScriptDecodingDirections.WrongfulAcceptance;

export const toError = (cause: unknown): Error =>
  cause instanceof Error ? cause : new Error(String(cause));
