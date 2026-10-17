import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { MpfError } from "../../mpf/index.js";
import { type RootProofVerificationOptions } from "./transition-roots.validate-count-proof.js";
import {
  verifyRootMembershipProof,
  verifyRootNonMembershipProof,
} from "./transition-roots.verify-indexed-trace-proof.js";

export const verifyEventToStepProof = (
  witness: SDK.EventToStepProof,
  options: Omit<RootProofVerificationOptions, "expectedDomain">,
): Effect.Effect<void, MpfError, never> => {
  if ("EventToStepMembership" in witness) {
    return verifyRootMembershipProof({
      witness: witness.EventToStepMembership.membership,
      keySchema: SDK.EventKeySchema,
      valueSchema: SDK.EventToStepValueSchema,
      options: {
        ...options,
        expectedDomain: SDK.ROOT_DOMAINS.eventToStep,
      },
    });
  }
  return verifyRootNonMembershipProof({
    witness: witness.EventToStepNonMembership.non_membership,
    keySchema: SDK.EventKeySchema,
    options: {
      ...options,
      expectedDomain: SDK.ROOT_DOMAINS.eventToStep,
    },
  });
};
