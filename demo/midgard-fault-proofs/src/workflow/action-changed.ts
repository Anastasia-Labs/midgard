/** The next unsigned action changed under a fresh authenticated L1 observation. */
export class WorkflowActionChangedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "WorkflowActionChangedError";
  }
}
