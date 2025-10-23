import bodyParser from "body-parser";
import { app } from "mu";

app.use(bodyParser.json({ type: '*/*' }));

app.post("/delta", async (req, res) => {
  console.log("Received delta", JSON.stringify(req.body, null, 2));

  console.log("Headers:", req.headers);
  console.log("Raw body:", req.rawBody);
  console.log("Parsed body:", JSON.stringify(req.body, null, 2));

  const inserts = req.body.delta?.[0]?.inserts || [];
  const reviewTriples = inserts.filter(t => t.predicate.value === "http://schema.org/about");

  for (const triple of reviewTriples) {
    const reviewUri = triple.subject.value;
    const bookUri = triple.object.value;

    console.log(`Review ${reviewUri} is about book ${bookUri}`);

    const reviewsResponse = await fetch(
      `http://resource/reviews?filter[book]=${encodeURIComponent(bookUri)}`
    );
    const reviewsJson = await reviewsResponse.json();

    const ratings = reviewsJson.data.map(r => r.attributes["review-rating"]);
    const avg = ratings.reduce((a, b) => a + b, 0) / ratings.length;

    const bookId = bookUri.split("/").pop();
    await fetch(`http://resource/books/${bookId}`, {
      method: "PATCH",
      headers: {
        "Content-Type": "application/vnd.api+json",
      },
      body: JSON.stringify({
        data: {
          type: "books",
          id: bookId,
          attributes: {
            "average-rating": avg,
          },
        },
      }),
    });

    console.log(`Updated book ${bookId} average rating to ${avg}`);
  }

  res.sendStatus(200);
});

app.listen(3000, () => console.log("rating-service listening on port 3000"));
