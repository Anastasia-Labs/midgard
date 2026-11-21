/** The validation-trace dispute at the CEK Data scalar steps of an integer
 * past the direct payload bound, on the emulator: unIData of an integer
 * leaf of 10,001 magnitude bytes and iData of such an integer
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
  DATA_SCALAR_TAG,
  INTEGER_SCRIPT,
} from "./support/validation-trace-dispute-data-scalar-step.js";
import { TIMEOUT } from "./support/validation-trace-dispute-max-shape-case.js";

describe("validation-trace dispute: one-step resolution of the CEK Data scalar steps of an integer past the direct payload bound", () => {
  const journey = dataScalarJourney({
    script: INTEGER_SCRIPT,
    first: DATA_SCALAR_TAG.unIData,
    second: DATA_SCALAR_TAG.iData,
  });

  it("disputes two Data scalar steps over values past the direct payload bound", () =>
    presentsTheSteps(journey));

  it("brings each thread of each second case to ReadyForOneStep on the step, with the committed successor the challenger's", () =>
    readiesTheSecondCases(journey));

  it(
    "a correct post state is not convicted: the Data scalar arm refuses unIData of an integer leaf past the bound returning the result of another leaf",
    () => refusesAtTheArm(journey, DATA_SCALAR_TAG.unIData),
    TIMEOUT,
  );

  it(
    "a correct post state is not convicted: the Data scalar arm refuses iData of an integer past the bound returning the result of another leaf",
    () => refusesAtTheArm(journey, DATA_SCALAR_TAG.iData),
    TIMEOUT,
  );

  it(
    "a wrong post state is convicted: bisects both threads of our case, resolves each through the Data scalar arm and closes the case Convicted with our bond back",
    () => convictsOurCase(journey),
    TIMEOUT,
  );
});
