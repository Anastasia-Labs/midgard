/**
 * The mint-authorization builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-steps.ts).
 */
import { buildMintAuthorizationStep01 } from "../../src/mint-authorization/submit-mint-authorization-step-01.js";
import { buildMintAuthorizationStep02 } from "../../src/mint-authorization/submit-mint-authorization-step-02.submit-mint-authorization-step02.js";
import { buildMintAuthorizationStep03WitnessAbsence } from "../../src/mint-authorization/submit-mint-authorization-step-03.submit-mint-authorization-step03-witness-absence.js";
import {
  buildMintAuthorizationStep04AdvanceComplete,
  buildMintAuthorizationStep04ResolveNext,
} from "../../src/mint-authorization/submit-mint-authorization-step-04.js";
import { buildMintAuthorizationStep05 } from "../../src/mint-authorization/submit-mint-authorization-step-05.js";
import { stepSubmitter, turnsSubmitter } from "./submit-steps.js";

export const submitMintAuthorizationStep01 = stepSubmitter(
  buildMintAuthorizationStep01,
);
export const submitMintAuthorizationStep02 = turnsSubmitter(
  buildMintAuthorizationStep02,
);
export const submitMintAuthorizationStep03WitnessAbsence = turnsSubmitter(
  buildMintAuthorizationStep03WitnessAbsence,
);
export const submitMintAuthorizationStep04AdvanceComplete = turnsSubmitter(
  buildMintAuthorizationStep04AdvanceComplete,
);
export const submitMintAuthorizationStep04ResolveNext = turnsSubmitter(
  buildMintAuthorizationStep04ResolveNext,
);
export const submitMintAuthorizationStep05 = stepSubmitter(
  buildMintAuthorizationStep05,
);
