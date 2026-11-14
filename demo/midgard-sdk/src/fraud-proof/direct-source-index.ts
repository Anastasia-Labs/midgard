import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import { DirectScriptSourceSchema } from "./direct-script-derivation.js";

export {
  DirectSourceIndex,
  DirectSourceIndexSchema,
} from "./direct-source-index-state.js";
export const DirectSourceAdmissionSchema = Data.Enum([
  Data.Object({ First: Data.Object({ proof: ProofSchema }) }),
  Data.Object({
    Existing: Data.Object({
      previous: DirectScriptSourceSchema,
      proof: ProofSchema,
    }),
  }),
]);
export type DirectSourceAdmission = Data.Static<
  typeof DirectSourceAdmissionSchema
>;
export const DirectSourceAdmission = asDataType<DirectSourceAdmission>(
  DirectSourceAdmissionSchema,
);
