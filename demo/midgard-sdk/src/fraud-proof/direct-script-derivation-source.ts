/** Wire twin of N6 SourceBoundV1, written only by authenticated step01. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { HeaderSchema } from "../ledger-state.js";
import { VerdictSubjectSchema } from "./proof-thread-substrate.js";
export const DirectScriptSourceBoundSchema = Data.Object({
  subject: VerdictSubjectSchema,
  header: HeaderSchema,
  compact_cbor: Data.Bytes(),
});
export type DirectScriptSourceBound = Data.Static<
  typeof DirectScriptSourceBoundSchema
>;
export const DirectScriptSourceBound = asDataType<DirectScriptSourceBound>(
  DirectScriptSourceBoundSchema,
);
