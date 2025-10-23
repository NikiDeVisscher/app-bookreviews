import bodyParser from "body-parser";
import { app } from "mu";

app.use(bodyParser.json({ type: '*/*' }));

app.post("/delta", async (req, res) => {
  console.log("Received delta", JSON.stringify(req.body, null, 2));

  const deltas = Array.isArray(req.body)
    ? req.body
    : req.body.delta || [];

  const inserts = deltas.flatMap(d => d.inserts || []);
  const reviewTriples = inserts.filter(
    t => t.predicate.value === "http://schema.org/about"
  );

  for (const triple of reviewTriples) {
    const reviewUri = triple.subject.value;
    const bookUri = triple.object.value;
    const bookId = bookUri.split("/").pop();

    try {
      const reviewsResponse = await fetch(
        `http://resource/books/${bookId}/reviews`,
      );

      if (!reviewsResponse.ok) {
        console.error(`Failed to fetch reviews for book ${bookUri}:`, reviewsResponse.status, reviewsResponse.statusText);
        continue;
      }

      const reviewsJson = await reviewsResponse.json();

      if (!reviewsJson.data || !Array.isArray(reviewsJson.data)) {
        console.error(`Invalid response for book ${bookUri}:`, JSON.stringify(reviewsJson, null, 2));
        continue;
      }
    
      const ratings = reviewsJson.data
        .map(r => r.attributes["reviewrating"])
        .filter(v => typeof v === "number");
    
      if (ratings.length === 0) {
        console.log(`No ratings found for ${bookUri}, skipping.`);
        continue;
      }
    
      const avg = ratings.reduce((a, b) => a + b, 0) / ratings.length;
    
      await fetch(`http://resource/books/${bookId}`, {
        method: "PATCH",
        headers: { "Content-Type": "application/vnd.api+json" },
        body: JSON.stringify({
          data: {
            type: "books",
            id: bookId,
            attributes: { "averagerating": avg },
          },
        }),
      });
    
      console.log(`Updated book ${bookId} average rating to ${avg}`);
    } catch (error) {
      console.error(`Error processing review ${reviewUri} for book ${bookUri}:`, error);
    }
  }  

  res.sendStatus(200);
});

app.listen(3000, () => console.log("rating-service listening on port 3000"));
