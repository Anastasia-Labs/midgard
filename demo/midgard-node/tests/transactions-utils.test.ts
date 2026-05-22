import { describe, expect, it } from "vitest";

import { parseOutsideValidityIntervalDetails } from "@/transactions/utils.js";

describe("parseOutsideValidityIntervalDetails", () => {
  it("parses escaped Kupmios/Ogmios early-validity submit errors", () => {
    const message =
      'KupmiosError: ResponseError: {"jsonrpc":"2.0","method":"submitTransaction","error":{"code":3118,"message":"The transaction is outside of its validity interval.","data":{"validityInterval":{"invalidBefore":123415253,"invalidAfter":123415372},"currentSlot":123415249}},"id":null}'.replace(
        /"/g,
        '\\"',
      );

    expect(parseOutsideValidityIntervalDetails(message)).toEqual({
      invalidBeforeSlot: 123415253,
      invalidHereafterSlot: 123415372,
      currentSlot: 123415249,
    });
  });
});
