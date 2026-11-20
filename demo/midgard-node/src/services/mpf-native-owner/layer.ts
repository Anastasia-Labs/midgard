import { Context, Effect, Layer } from "effect";

import {
  ProductionNativeMpfOwnerService,
  type NativeMpfOwnerServiceOptions,
} from "./service.js";
import type { NativeMpfOwnerService } from "./protocol.js";

/** Main-process ownership boundary for the sole Architecture G Level lock. */
export class NativeMpfOwner extends Context.Tag("NativeMpfOwner")<
  NativeMpfOwner,
  NativeMpfOwnerService
>() {}

export const makeNativeMpfOwnerLayer = (
  options: NativeMpfOwnerServiceOptions,
) =>
  Layer.scoped(
    NativeMpfOwner,
    Effect.acquireRelease(
      Effect.tryPromise(() => ProductionNativeMpfOwnerService.create(options)),
      (owner) =>
        Effect.promise(() => owner.close()).pipe(
          Effect.catchAll(() => Effect.void),
        ),
    ),
  );
