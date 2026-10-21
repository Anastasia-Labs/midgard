import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

/**
 * The validation-trace validators' scenarios: the dispute bond, and the
 * case, game, award, boundary, thread-expiry and timeout validators of the
 * validation case. `VALIDATOR_SCENARIOS` spreads them in at their place.
 */
export const VALIDATION_TRACE_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/validation_trace/dispute_bond_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "refunds the bond to the challenger when its case convicts",
      },
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "forfeits the bond to the treasury when a defender won, and refuses paying it to the challenger",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "refuses a bond spent without a case close",
      },
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "refuses a case close that takes another case's bond",
      },
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "refuses a case close over a bond without the case's marker",
      },
    ],
  },
  "fraud_proofs/validation_trace/award_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "closes a defender's timeout as the challenger's win, minting the receipt from the thread alone",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "refuses a receipt minted without a case input",
      },
    ],
  },
  "fraud_proofs/validation_trace/boundary_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "enters resolution and prepares it only with the fraud prover's signature",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "enters resolution and prepares it only with the fraud prover's signature",
      },
    ],
  },
  "fraud_proofs/validation_trace/case_config_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "opens a case and its bond from the source, and refuses an opener elsewhere",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "refuses a second configuration once its initialization UTxO is spent",
      },
    ],
  },
  "fraud_proofs/validation_trace/case_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "accepts a signed active operator and holds its bond through the case",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "convicts an undefended case only after the defense deadline, refunding the bond",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "refuses an unsigned defense, an inactive operator and a thread short of its reserve",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "couples the thread token to the defense: a Defend minting none or two, and a mint without a Defend, are refused",
      },
    ],
  },
  "fraud_proofs/validation_trace/game_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "lets a third party make both reveals, each paying a capped fee from the thread",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "moves two threads in one transaction only when each redeemer names its own successor",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "refuses a lone thread input paying more than the cap, and a second input taking thread value",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "enters resolution and prepares it only with the fraud prover's signature",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "refuses a challenger timeout until the operator's clock has run out, then lets anyone hand the thread to the timeout",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "charges a long gap inside the clock to the mover, exactly, and convicts no one",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "charges nothing for a lower bound before the turn start, and charges from the previous upper bound",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "refuses a validity range longer than the dispute maximum",
      },
      {
        file: "demo/midgard-fault-proofs/tests/validation-game-emulator.test.ts",
        test: "moves two threads in one transaction only when each redeemer names its own successor",
      },
    ],
  },
  "fraud_proofs/validation_trace/thread_expiry_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "releases a live thread to its defender once the longest case is over, and not before",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "releases a live thread to its defender once the longest case is over, and not before",
      },
    ],
  },
  "fraud_proofs/validation_trace/timeout_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "closes a defender's timeout as the challenger's win, minting the receipt from the thread alone",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-case-emulator.test.ts",
        test: "refuses a close naming the other side as the winner",
      },
    ],
  },
};

/**
 * The dispute family: a failing script's block convicted and removed, and the
 * case refusing the challenger once the honest operator defends.
 */
export const VALIDATION_TRACE_DISPUTE_FAMILY_SCENARIOS: ValidatorScenarioPair =
  {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-trace-dispute-conviction.test.ts",
        test: "removes the convicted block and its descendants through the shared removal once the L1 view has seen the record, keeping the record, and completes on the authenticated terminal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-trace-dispute-defend.test.ts",
        test: "refuses a Defend that carries the challenger's own Terminal Rejected state",
      },
      {
        file: "demo/midgard-fault-proofs/tests/dispute-bond-emulator.test.ts",
        test: "forfeits the bond to the treasury when a defender won, and refuses paying it to the challenger",
      },
    ],
  };
