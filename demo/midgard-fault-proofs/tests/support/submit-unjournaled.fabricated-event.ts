/**
 * The fabricated-deposit and fabricated-withdrawal builders the tests drive,
 * each bound into a function that builds its step and submits it with no
 * journal (see ./submit-unjournaled.ts).
 */
import { buildFabricatedDepositStep02 } from "../../src/submit-fabricated-deposit-step-02.js";
import { buildFabricatedWithdrawalStep02 } from "../../src/submit-fabricated-withdrawal-step-02.js";
import { unjournaled } from "./submit-unjournaled.js";

export const submitFabricatedDepositStep02 = unjournaled(
  buildFabricatedDepositStep02,
);
export const submitFabricatedWithdrawalStep02 = unjournaled(
  buildFabricatedWithdrawalStep02,
);
