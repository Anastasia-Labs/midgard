import {
  buildMidgardNativeScriptDecodingTrace,
  MidgardNativeScriptDecodingTraceOutcomeKinds,
} from "./native-script-decoding-engine.build-midgard-native-script-decoding-trace.js";
import {
  MidgardNativeScriptDecodingBindKinds,
  MidgardNativeScriptDecodingRefusalClasses,
  parseMidgardVersionedScriptHeader,
} from "./native-script-decoding-engine.parse-midgard-versioned-script-header.js";
import { encodeMidgardNativeScriptStructureControl } from "./native-script-scan.js";

/**
 * How the witness-script decoding proof classifies one script-witness item
 * (field 6): the class its on-chain rule establishes for the whole item.
 * `noFault` covers a sound native script and every non-native language.
 */
export type MidgardWitnessScriptItemClass =
  | "noFault"
  | "headerMalformed"
  | "nativeMalformed"
  | "nodeLimit"
  | "depthLimit";

export type MidgardWitnessScriptItemClassification = {
  readonly itemClass: MidgardWitnessScriptItemClass;
  /** The native scan's initial control, or "" when no native scan binds. */
  readonly initialControlCbor: string;
};

/**
 * The single classifier the proof twin and the node's rejection subject share,
 * so the item a rejection names is the one the proof finds faulty.
 */
export const classifyMidgardWitnessScriptItem = (
  item: Uint8Array,
): MidgardWitnessScriptItemClassification => {
  const header = parseMidgardVersionedScriptHeader(item, item.length);
  if (header === null) {
    return { itemClass: "headerMalformed", initialControlCbor: "" };
  }
  if (header.languageTag !== 0) {
    return { itemClass: "noFault", initialControlCbor: "" };
  }
  const trace = buildMidgardNativeScriptDecodingTrace(item);
  if (trace.bind.kind === MidgardNativeScriptDecodingBindKinds.Malformed) {
    // A parsed tag-0 wrapper can only land here for the empty payload. That is
    // a payload structural failure, not a header failure.
    return { itemClass: "nativeMalformed", initialControlCbor: "" };
  }
  if (trace.bind.kind !== MidgardNativeScriptDecodingBindKinds.Bound) {
    throw new Error(
      "witness script item: native header produced an impossible non-native trace",
    );
  }
  const initialControlCbor = encodeMidgardNativeScriptStructureControl(
    trace.bind.control,
  ).toString("hex");
  if (trace.outcome === null) {
    throw new Error("witness script item: native scan has no terminal outcome");
  }
  if (
    trace.outcome.kind === MidgardNativeScriptDecodingTraceOutcomeKinds.Terminal
  ) {
    return { itemClass: "noFault", initialControlCbor };
  }
  const itemClass =
    trace.outcome.refusalClass ===
    MidgardNativeScriptDecodingRefusalClasses.Malformed
      ? "nativeMalformed"
      : trace.outcome.refusalClass ===
          MidgardNativeScriptDecodingRefusalClasses.NodeLimit
        ? "nodeLimit"
        : "depthLimit";
  return { itemClass, initialControlCbor };
};
