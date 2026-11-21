import { timingSafeEqual } from "node:crypto";

import { NATIVE_MPF_OWNER_DEFAULT_CAPS } from "./protocol.js";
import {
  digest,
  EVENT_LOG_HEADER_BYTES,
  EVENT_STREAM_DIGEST_DOMAIN,
  FULL_INDEX_MAX_BYTES,
} from "./service.normalize-owner-options.js";

export const validateEventLog = (baseRoot: string, log: Buffer): number => {
  if (
    log.length < EVENT_LOG_HEADER_BYTES ||
    log.subarray(0, 4).toString("ascii") !== "MEGO" ||
    log.readUInt16LE(4) !== 1 ||
    log.readUInt16LE(6) !== 0 ||
    log.subarray(28, 60).toString("hex") !== baseRoot
  ) {
    throw new Error("Native MPF event log header/base is invalid");
  }
  const eventCount = log.readUInt32LE(8);
  const opCount = log.readUInt32LE(12);
  if (
    eventCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxEvents ||
    opCount > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxOps ||
    log.length > FULL_INDEX_MAX_BYTES
  ) {
    throw new Error("Native MPF event log cap exceeded");
  }
  const expected = digest(
    EVENT_STREAM_DIGEST_DOMAIN,
    log.subarray(8, 28),
    log.subarray(28, 60),
    log.subarray(EVENT_LOG_HEADER_BYTES),
  );
  if (!timingSafeEqual(expected, log.subarray(60, EVENT_LOG_HEADER_BYTES))) {
    throw new Error("Native MPF event log digest mismatch");
  }
  return eventCount;
};
