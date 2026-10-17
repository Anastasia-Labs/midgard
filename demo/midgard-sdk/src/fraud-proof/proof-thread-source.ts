/** Direction-free public source carriage; authentication occurs on chain. */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { OutputReferenceSchema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { NativeTxInclusionCarriageSchema } from "./native.js";
export const ProofThreadForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
});
export const ProofThreadSubjectSourceSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
    }),
  }),
]);
export type ProofThreadSubjectSource = Data.Static<
  typeof ProofThreadSubjectSourceSchema
>;
export const ProofThreadSubjectSource = asDataType<ProofThreadSubjectSource>(
  ProofThreadSubjectSourceSchema,
);
