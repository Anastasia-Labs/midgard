/**
 * The mint-authorization builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import {
  buildMintAuthorizationStep01,
  buildMintAuthorizationStep02,
  buildMintAuthorizationStep03WitnessAbsence,
  buildMintAuthorizationStep04AdvanceComplete,
  buildMintAuthorizationStep04ResolveNext,
  buildMintAuthorizationStep05,
} from "../../src/mint-authorization/index.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitMintAuthorizationStep01 = unjournaled(
  buildMintAuthorizationStep01,
);
export const submitMintAuthorizationStep02 = unjournaledTurns(
  buildMintAuthorizationStep02,
);
export const submitMintAuthorizationStep03WitnessAbsence = unjournaledTurns(
  buildMintAuthorizationStep03WitnessAbsence,
);
export const submitMintAuthorizationStep04AdvanceComplete = unjournaledTurns(
  buildMintAuthorizationStep04AdvanceComplete,
);
export const submitMintAuthorizationStep04ResolveNext = unjournaledTurns(
  buildMintAuthorizationStep04ResolveNext,
);
export const submitMintAuthorizationStep05 = unjournaled(
  buildMintAuthorizationStep05,
);
