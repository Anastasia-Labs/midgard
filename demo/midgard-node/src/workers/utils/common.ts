import { Data } from "effect";
import * as SDK from "@al-ft/midgard-sdk";

export class WorkerError extends Data.TaggedError("WorkerError")<
  SDK.Utils.GenericErrorFields & {
    readonly worker: string;
  }
> {}
