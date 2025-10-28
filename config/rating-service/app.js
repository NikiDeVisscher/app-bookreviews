import bodyParser from "body-parser";
import { app } from "mu";

app.use(bodyParser.json({ type: "*/*" }));

app.post("/delta", async (req, res) => {
  console.log("Received delta", JSON.stringify(req.body, null, 2));

  const deltas = Array.isArray(req.body)
    ? req.body
    : req.body.delta || [];

  const insertsTriples = deltas.flatMap(d => d.inserts || []).filter(
    t => t.predicate.value === "http://schema.org/reviewRating"
  );
  const deletesTriples = deltas.flatMap(d => d.deletes || []).filter(
    t => t.predicate.value === "http://schema.org/about"
  );

  let reviewUri = null;
  let bookId = null;

  for (const triple of insertsTriples) {
    reviewUri = triple.subject.value;
    let reviewId = reviewUri.split("/").pop();
    let bookId = await fetchBookId(reviewId);
    if (bookId) {
      await updateRating(bookId);
    }
  }

  for (const triple of deletesTriples) {
    if (!insertsTriples.find(t => t.subject.value === triple.subject.value)) {
      reviewUri = triple.subject.value;
      let bookUri = triple.object.value;
      bookId = bookUri.split("/").pop();
      await updateRating(bookId);
    }
  }

  res.sendStatus(200);
});

/** Fetches the linked book and reads the id. */
async function fetchBookId(reviewId) {
  let bookId = null;

  try {
    const bookUriResponse = await fetch(
      `http://resource/reviews/${reviewId}/book`
    );

    if (!bookUriResponse.ok) {
      console.error(
        `Failed to fetch book for review ${reviewId}:`,
        bookUriResponse.status,
        bookUriResponse.statusText
      );
      return false;
    }

    const bookUriJson = await bookUriResponse.json();

    if (!bookUriJson.data || !bookUriJson.data.attributes.uri) {
      console.error(
        `Invalid response for review ${reviewId}:`,
        JSON.stringify(bookUriJson, null, 2)
      );
      return false;
    }

    const bookUri = bookUriJson.data.attributes.uri;
    bookId = bookUri.split("/").pop();
  } catch (error) {
    console.error(`Error fetching book for review ${reviewId}:`, error);
    return false;
  }

  return bookId;
}

/** Fetches the ratings of all reviews for a book, calculates the average and patches the new average rating. */
async function updateRating(bookId) {
  try {
    console.log(`Recalculating average for book ${bookId}`);

    const reviewsResponse = await fetch(
      `http://resource/books/${bookId}/reviews`
    );

    if (!reviewsResponse.ok) {
      console.error(
        `Failed to fetch reviews for book ${bookId}:`,
        reviewsResponse.status,
        reviewsResponse.statusText
      );
      return false;
    }

    const reviewsJson = await reviewsResponse.json();

    if (!reviewsJson.data || !Array.isArray(reviewsJson.data)) {
      console.error(
        `Invalid response for book ${bookId}:`,
        JSON.stringify(reviewsJson, null, 2)
      );
      return false;
    }

    const ratings = reviewsJson.data
      .map(r => r.attributes["reviewrating"])
      .filter(v => typeof v === "number");

    const avg =
      ratings.length > 0
        ? ratings.reduce((a, b) => a + b, 0) / ratings.length
        : null;

    await fetch(`http://resource/books/${bookId}`, {
      method: "PATCH",
      headers: { "Content-Type": "application/vnd.api+json" },
      body: JSON.stringify({
        data: {
          type: "books",
          id: bookId,
          attributes: { averagerating: avg },
        },
      }),
    });

    console.log(`Updated book ${bookId} average rating to ${avg}`);
  } catch (error) {
    console.error(`Error processing review for book ${bookId}:`, error);
  }

  return true;
}

app.listen(3000, () => console.log("rating-service listening on port 3000"));
