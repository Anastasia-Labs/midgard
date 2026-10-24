import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";

/** Shared source-map receipt; independent of the source admission payloads. */
export const DirectSourceIndexSchema = Data.Object({
  root: H32Schema,
  count: Data.Integer(),
});
export type DirectSourceIndex = Data.Static<typeof DirectSourceIndexSchema>;
export const DirectSourceIndex = asDataType<DirectSourceIndex>(
  DirectSourceIndexSchema,
);
