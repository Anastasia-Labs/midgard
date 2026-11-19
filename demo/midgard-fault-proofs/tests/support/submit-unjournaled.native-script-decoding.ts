/**
 * The native-script-decoding builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import { buildNativeScriptDecodingInit } from "../../src/native-script-decoding/submit-native-script-decoding-init.js";
import {
  buildNativeScriptDecodingStep01BindNormal,
  buildNativeScriptDecodingStep01RecordForced,
} from "../../src/native-script-decoding/submit-native-script-decoding-step-01.js";
import { buildNativeScriptDecodingStep02 } from "../../src/native-script-decoding/submit-native-script-decoding-step-02.js";
import {
  buildNativeScriptDecodingStep03AdvanceOrCloseClose,
  buildNativeScriptDecodingStep03AdvanceOrCloseSegment,
  buildNativeScriptDecodingStep03BindDescriptor,
  buildNativeScriptDecodingStep03OpenSubject,
} from "../../src/native-script-decoding/submit-native-script-decoding-step-03.js";
import { buildNativeScriptDecodingStep04 } from "../../src/native-script-decoding/submit-native-script-decoding-step-04.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitNativeScriptDecodingInit = unjournaled(
  buildNativeScriptDecodingInit,
);
export const submitNativeScriptDecodingStep01BindNormal = unjournaled(
  buildNativeScriptDecodingStep01BindNormal,
);
export const submitNativeScriptDecodingStep01RecordForced = unjournaled(
  buildNativeScriptDecodingStep01RecordForced,
);
export const submitNativeScriptDecodingStep02 = unjournaled(
  buildNativeScriptDecodingStep02,
);
export const submitNativeScriptDecodingStep03AdvanceOrCloseClose = unjournaled(
  buildNativeScriptDecodingStep03AdvanceOrCloseClose,
);
export const submitNativeScriptDecodingStep03AdvanceOrCloseSegment =
  unjournaled(buildNativeScriptDecodingStep03AdvanceOrCloseSegment);
export const submitNativeScriptDecodingStep03BindDescriptor = unjournaled(
  buildNativeScriptDecodingStep03BindDescriptor,
);
export const submitNativeScriptDecodingStep03OpenSubject = unjournaledTurns(
  buildNativeScriptDecodingStep03OpenSubject,
);
export const submitNativeScriptDecodingStep04 = unjournaled(
  buildNativeScriptDecodingStep04,
);
