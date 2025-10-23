export default [
  {
    match: {
      predicate: {
        type: "uri",
        value: "http://schema.org/reviewRating",
      },
    },
    callback: {
      url: "http://rating-service/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      predicate: {
        type: "uri",
        value: "http://schema.org/reviewContent",
      },
    },
    callback: {
      url: "http://rating-service/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      predicate: {
        type: "uri",
        value: "http://schema.org/about",
      },
    },
    callback: {
      url: "http://rating-service/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
];
