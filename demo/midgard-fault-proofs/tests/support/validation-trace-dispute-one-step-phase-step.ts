/** The rows of the validation-trace dispute runner at one phase step whose
 * successor's thread forks from our trace there: the runner routes the step
 * to the semantic resolver its evidence selects, which refuses any other
 * transition, and moves the thread through the resolution to the award
 * (validation-trace-dispute-one-step-phase-drive.ts). */
import { beforeAll, describe, it } from "vitest";

import { phaseDrive } from "./validation-trace-dispute-one-step-phase-drive.js";

/**
 * The rows of the step `role` of `drive`'s phase after our trace's state
 * `drive.from`; the drive returned for further rows. `refuses` declares,
 * inside the rows, the case that pins the step's semantic resolver
 * refusing `drive.otherTransition()`. That case and its `refusedBy` pin sit
 * in the test file, where the traced refusal run plans it and records it
 * (scripts/run-traced-refusals.mjs).
 */
export const describePhaseStep = (
  {
    role,
    ...options
  }: Parameters<typeof phaseDrive>[0] & { readonly role: string },
  refuses: (drive: ReturnType<typeof phaseDrive>) => void,
) => {
  const drive = phaseDrive(options);
  describe(`validation-trace dispute runner: driving a thread at the ${role} step after state ${options.from.toString()}`, () => {
    beforeAll(drive.setUp, 1_800_000);

    it(
      `routes the step through the phase resolver to the ${role} semantic resolver, committing the transition beside the evidence it takes`,
      drive.routes,
      300_000,
    );

    refuses(drive);

    it(
      "moves the thread through the step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );

    it(
      "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
      drive.pays,
    );
  });
  return drive;
};
