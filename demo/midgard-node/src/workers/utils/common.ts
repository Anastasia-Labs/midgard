import { Data } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * Base tagged error for worker and worker-helper failures.
 */
export class WorkerError extends Data.TaggedError("WorkerError")<
  SDK.GenericErrorFields & {
    readonly worker: string;
  }
> {}
