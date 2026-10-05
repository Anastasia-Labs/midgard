import { Effect } from "effect";

import { buildSharedFaultProofContracts } from "../shared.js";
import { buildValidationTraceDisputeChain } from "./validation-trace-dispute.build-validation-trace-dispute-chain.js";
import {
  type BuildValidationTraceDisputeFaultProofContractsParams,
  type ValidationTraceDisputeFaultProofContracts,
} from "./validation-trace-dispute.types.js";

export const buildValidationTraceDisputeFaultProofContracts = (
  params: BuildValidationTraceDisputeFaultProofContractsParams,
): Effect.Effect<ValidationTraceDisputeFaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });
    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fieldPreimageCertificate: shared.fieldPreimageCertificate,
      validationTraceDispute,
    };
  });
