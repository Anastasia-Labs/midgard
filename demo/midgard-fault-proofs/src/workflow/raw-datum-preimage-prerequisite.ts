import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import type { RawDatumPreimageRequirement } from "./raw-datum-preimage.js";
export {
  createRawDatumPreimageRequirement,
  createStructuredDataPreimageRequirement,
  rawDatumPreimagePublicationPlan,
  type RawDatumPreimageRequirement,
} from "./raw-datum-preimage.js";

/** Exact-L1 observation, signed capture, and restart recovery share one implementation. */
export const createAuthenticatedRawDatumPreimagePrerequisitePort = <
  Category extends FraudProofCatalogueCategoryName,
>(
  config: Omit<
    Parameters<
      typeof createAuthenticatedFieldCarriagePrerequisitePort<Category>
    >[0],
    "requirementForAction"
  > & {
    readonly requirementForAction: (
      input: Parameters<
        Parameters<
          typeof createAuthenticatedFieldCarriagePrerequisitePort<Category>
        >[0]["requirementForAction"]
      >[0],
    ) =>
      | RawDatumPreimageRequirement
      | null
      | Promise<RawDatumPreimageRequirement | null>;
  },
) => createAuthenticatedFieldCarriagePrerequisitePort(config);

export const withRawDatumPreimagePrerequisite = <
  Category extends FraudProofCatalogueCategoryName,
>(
  config: Omit<
    Parameters<typeof withFieldCarriagePrerequisite<Category>>[0],
    "rawDatum"
  >,
) => withFieldCarriagePrerequisite({ ...config, rawDatum: true });
