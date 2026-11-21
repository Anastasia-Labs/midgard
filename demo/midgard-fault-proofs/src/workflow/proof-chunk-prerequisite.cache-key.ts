export const cacheKey = (workflowId: string, actionId: string): string =>
  `${workflowId}\u0000${actionId}`;
