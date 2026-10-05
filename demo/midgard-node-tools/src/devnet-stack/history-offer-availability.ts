import type { HistoryWindowOffer } from "./history-child-evidence.js";
import type { HistoryWindowSeal } from "./history-native-window-proof.js";
import {
  publication,
  rangeAt,
  rangeDigest,
  rowAt,
} from "./history-window-canonical.js";

/** Entire exact range and direct predecessor must be coherently serveable now. */
export const historyOfferAvailable = (
  directories: readonly string[],
  offer: HistoryWindowOffer,
): boolean => {
  try {
    const before = publication(directories);
    const { window, predecessor } = offer;
    const first = BigInt(window.first.blockNo);
    const rows = rangeAt(directories, first, BigInt(window.last.blockNo));
    const start = rows[0];
    const end = rows[rows.length - 1];
    if (
      start === undefined ||
      end === undefined ||
      rows.length !== window.rowCount ||
      JSON.stringify(start.point) !== JSON.stringify(window.first) ||
      JSON.stringify(end.point) !== JSON.stringify(window.last) ||
      rangeDigest(rows) !== window.rangeDigest
    )
      return false;
    if (first === 0n) {
      if (predecessor !== null || start.prevHash !== "") return false;
    } else {
      if (predecessor === null) return false;
      const previous = rowAt(directories, first - 1n);
      if (
        JSON.stringify(previous.point) !== JSON.stringify(predecessor) ||
        previous.point.blockHash !== start.prevHash ||
        BigInt(previous.point.slot) >= BigInt(start.point.slot)
      )
        return false;
    }
    return publication(directories) === before;
  } catch {
    // Missing/torn evidence never clears a hold; I/O failures are not exit78.
    return false;
  }
};

/** Native authority comes from the caller's live sealer, never from these files. */
export const offerForHistorySeal = (
  directories: readonly string[],
  window: HistoryWindowSeal,
): HistoryWindowOffer | null => {
  try {
    const first = BigInt(window.first.blockNo);
    const offer = {
      window,
      predecessor: first === 0n ? null : rowAt(directories, first - 1n).point,
    };
    return historyOfferAvailable(directories, offer) ? offer : null;
  } catch {
    return null;
  }
};
