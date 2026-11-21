import type * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "effect";

/**
 * Base tagged error for worker and worker-helper failures.
 */
export class WorkerError extends Data.TaggedError("WorkerError")<
  SDK.GenericErrorFields & {
    readonly worker: string;
  }
> {}
