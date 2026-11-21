import {
  EXIT_COMPLETE,
  EXIT_GAPS,
} from "./family-checklist.top-level-elements.mjs";

export const exitCodeFor = (results) =>
  results.some((result) => result.status === "gap") ? EXIT_GAPS : EXIT_COMPLETE;

export const USAGE =
  "usage: family-checklist.mjs <category> [--root <repository-root>]";
