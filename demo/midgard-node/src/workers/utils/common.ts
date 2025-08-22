import { Data } from "effect";

export class WorkerError extends Data.TaggedError("SerializationError")<{
  readonly worker: string;
  readonly message: string;
  readonly cause?: unknown;
}> {}
