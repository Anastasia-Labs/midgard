/** Current raw physical producer retains validator-visible adversarial evidence. */
import { forcedInputExistenceMaterial } from "../../src/input-existence/reopen-forced.js";
import type { PreparedNonExistentInputWrongfulRejection } from "../../src/non-existent-input/wrongful-rejection.js";
import { submitRawInputExistenceStep } from "./raw-input-existence-step.js";

type RawArguments = Omit<
  Parameters<typeof submitRawInputExistenceStep>[0],
  "prepared" | "referenceScripts"
> & {
  readonly prepared: PreparedNonExistentInputWrongfulRejection;
  readonly references: Parameters<
    typeof submitRawInputExistenceStep
  >[0]["referenceScripts"];
};
export const submitRawNonExistentInputForcedStep = ({
  prepared,
  references,
  ...args
}: RawArguments) =>
  submitRawInputExistenceStep({
    ...args,
    referenceScripts: references,
    prepared: forcedInputExistenceMaterial(
      prepared,
      0,
      args.signer.paymentKeyHash,
    ),
  });
