/** The validation-trace dispute at the CEK Data scalar steps of a byte
 * string past the direct payload bound, on the emulator: unBData of a
 * 12,000-byte leaf and bData of a 12,000-byte string
 * (support/validation-trace-dispute-data-scalar-journey.ts). */
import { describe, it } from "vitest";

import {
  convictsOurCase,
  dataScalarJourney,
  presentsTheSteps,
  readiesTheSecondCases,
  refusesAtTheArm,
} from "./support/validation-trace-dispute-data-scalar-journey.js";
import {
  BYTES_SCRIPT,
  DATA_SCALAR_TAG,
} from "./support/validation-trace-dispute-data-scalar-step.js";
import { TIMEOUT } from "./support/validation-trace-dispute-max-shape-case.js";

describe("validation-trace dispute: one-step resolution of the CEK Data scalar steps of a 12,000-byte string", () => {
  const journey = dataScalarJourney({
    script: BYTES_SCRIPT,
    first: DATA_SCALAR_TAG.unBData,
    second: DATA_SCALAR_TAG.bData,
  });

  it("disputes two Data scalar steps over values past the direct payload bound", () =>
    presentsTheSteps(journey));

  it("brings each thread of each second case to ReadyForOneStep on the step, with the committed successor the challenger's", () =>
    readiesTheSecondCases(journey));

  it(
    "a correct post state is not convicted: the Data scalar arm refuses unBData of a 12,000-byte leaf returning the result of another leaf",
    () => refusesAtTheArm(journey, DATA_SCALAR_TAG.unBData),
    TIMEOUT,
  );

  it(
    "a correct post state is not convicted: the Data scalar arm refuses bData of a 12,000-byte string returning the result of another leaf",
    () => refusesAtTheArm(journey, DATA_SCALAR_TAG.bData),
    TIMEOUT,
  );

  it(
    "a wrong post state is convicted: bisects both threads of our case, resolves each through the Data scalar arm and closes the case Convicted with our bond back",
    () => convictsOurCase(journey),
    TIMEOUT,
  );
});
