/**
 * The native-script-decoding builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-steps.ts).
 */
import { buildNativeScriptDecodingInit } from "../../src/native-script-decoding/submit-native-script-decoding-init.js";
import { buildNativeScriptDecodingStep01BindNormal } from "../../src/native-script-decoding/submit-native-script-decoding-step-01.submit-native-script-decoding-step01-bind-normal.js";
import { buildNativeScriptDecodingStep01RecordForced } from "../../src/native-script-decoding/submit-native-script-decoding-step-01.submit-native-script-decoding-step01-record-forced.js";
import { buildNativeScriptDecodingStep02 } from "../../src/native-script-decoding/submit-native-script-decoding-step-02.js";
import {
  buildNativeScriptDecodingStep03AdvanceOrCloseClose,
  buildNativeScriptDecodingStep03AdvanceOrCloseSegment,
} from "../../src/native-script-decoding/submit-native-script-decoding-step-03.submit-native-script-decoding-step03-advance-or-close-close.js";
import { buildNativeScriptDecodingStep03BindDescriptor } from "../../src/native-script-decoding/submit-native-script-decoding-step-03.submit-native-script-decoding-step03-bind-descriptor.js";
import { buildNativeScriptDecodingStep03OpenSubject } from "../../src/native-script-decoding/submit-native-script-decoding-step-03.submit-native-script-decoding-step03-open-subject.js";
import { buildNativeScriptDecodingStep04 } from "../../src/native-script-decoding/submit-native-script-decoding-step-04.js";
import { stepSubmitter, turnsSubmitter } from "./submit-steps.js";

export const submitNativeScriptDecodingInit = stepSubmitter(
  buildNativeScriptDecodingInit,
);
export const submitNativeScriptDecodingStep01BindNormal = stepSubmitter(
  buildNativeScriptDecodingStep01BindNormal,
);
export const submitNativeScriptDecodingStep01RecordForced = stepSubmitter(
  buildNativeScriptDecodingStep01RecordForced,
);
export const submitNativeScriptDecodingStep02 = stepSubmitter(
  buildNativeScriptDecodingStep02,
);
export const submitNativeScriptDecodingStep03AdvanceOrCloseClose =
  stepSubmitter(buildNativeScriptDecodingStep03AdvanceOrCloseClose);
export const submitNativeScriptDecodingStep03AdvanceOrCloseSegment =
  stepSubmitter(buildNativeScriptDecodingStep03AdvanceOrCloseSegment);
export const submitNativeScriptDecodingStep03BindDescriptor = stepSubmitter(
  buildNativeScriptDecodingStep03BindDescriptor,
);
export const submitNativeScriptDecodingStep03OpenSubject = turnsSubmitter(
  buildNativeScriptDecodingStep03OpenSubject,
);
export const submitNativeScriptDecodingStep04 = stepSubmitter(
  buildNativeScriptDecodingStep04,
);
