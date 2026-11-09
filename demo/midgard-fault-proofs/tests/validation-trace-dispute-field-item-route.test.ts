/** The route a step's item read takes, and the room the certified route
 * takes from an observe move's inline carriage: none walking, and the
 * certificate's read through it. */
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { describe, expect, it } from "vitest";

import {
  VALIDATION_TRACE_CERTIFIED_ROUTE_MAX_EXTRA_BYTES,
  VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS,
  validationTraceFieldItemRoute,
  validationTraceObserveRouteExtraBytes,
} from "../src/validation-trace-dispute/one-step-field-item-route.js";

/** A field preimage of `count` items of `width` bytes each. */
const field = (count: number, width: number) =>
  encodeMidgardFieldPreimage(
    Array.from({ length: count }, (_, index) =>
      Buffer.alloc(width, index % 256),
    ),
  );

const OUTPUTS = 2;
const REQUIRED_SIGNERS = 4;
const MOST_WALKED = VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS;

describe("validation-trace field item route", () => {
  it("walks a variable-width field of at most the walking bound's items", () => {
    expect(
      validationTraceFieldItemRoute(
        { fieldIndex: OUTPUTS, certifiedOnly: false },
        field(MOST_WALKED, 40),
      ),
    ).toBe("walk");
  });

  it("reads a variable-width field past the walking bound through the certificate", () => {
    expect(
      validationTraceFieldItemRoute(
        { fieldIndex: OUTPUTS, certifiedOnly: false },
        field(MOST_WALKED + 1, 40),
      ),
    ).toBe("certified");
  });

  it("walks a fixed-stride field of any count, which has no certificate", () => {
    expect(
      validationTraceFieldItemRoute(
        { fieldIndex: REQUIRED_SIGNERS, certifiedOnly: false },
        field(MOST_WALKED + 1, 28),
      ),
    ).toBe("walk");
  });

  it("reads through the certificate on a stage that reads only through it", () => {
    expect(
      validationTraceFieldItemRoute(
        { fieldIndex: OUTPUTS, certifiedOnly: true },
        field(1, 40),
      ),
    ).toBe("certified");
  });

  it("takes no inline room from a walking observe", () => {
    expect(
      validationTraceObserveRouteExtraBytes(OUTPUTS, field(MOST_WALKED, 40)),
    ).toBe(0);
    expect(
      validationTraceObserveRouteExtraBytes(
        REQUIRED_SIGNERS,
        field(MOST_WALKED + 1, 28),
      ),
    ).toBe(0);
  });

  it("takes the certificate's read from a certified observe's inline room", () => {
    expect(
      validationTraceObserveRouteExtraBytes(
        OUTPUTS,
        field(MOST_WALKED + 1, 40),
      ),
    ).toBe(VALIDATION_TRACE_CERTIFIED_ROUTE_MAX_EXTRA_BYTES);
    // The route's index (up to 3 bytes over `WalkFieldItem`) and the
    // certificate's reference input (37 bytes, and up to 5 for a
    // reference-input set the move did not have).
    expect(VALIDATION_TRACE_CERTIFIED_ROUTE_MAX_EXTRA_BYTES).toBe(3 + 37 + 5);
  });
});
