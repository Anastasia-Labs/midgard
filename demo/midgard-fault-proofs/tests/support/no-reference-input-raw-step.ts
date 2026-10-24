/** Current raw physical producer retains validator-visible adversarial evidence. */
import { forcedInputExistenceMaterial } from "../../src/input-existence/reopen-forced.js";
import type { PreparedNoReferenceInputWrongfulRejection } from "../../src/no-reference-input/wrongful-rejection.js";
import { submitRawInputExistenceStep } from "./raw-input-existence-step.js";

type RawArguments = Omit<
  Parameters<typeof submitRawInputExistenceStep>[0],
  "prepared" | "referenceScripts"
> & {
  readonly prepared: PreparedNoReferenceInputWrongfulRejection;
  readonly references: Parameters<
    typeof submitRawInputExistenceStep
  >[0]["referenceScripts"];
};
export const submitRawNoReferenceInputForcedStep = ({
  prepared,
  references,
  ...args
}: RawArguments) =>
  submitRawInputExistenceStep({
    ...args,
    referenceScripts: references,
    prepared: forcedInputExistenceMaterial(
      prepared,
      1,
      args.signer.paymentKeyHash,
    ),
  });
