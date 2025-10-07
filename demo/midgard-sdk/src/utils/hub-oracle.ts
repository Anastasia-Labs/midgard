import { Data } from "effect";
import { GenericErrorFields } from "@/utils/common.js";

export class HubOracleError extends Data.TaggedError(
  "HubOracleError",
)<GenericErrorFields> {}
