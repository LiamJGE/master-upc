package exercise_4;

import com.clearspring.analytics.util.Lists;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.Dataset;
import org.apache.spark.sql.Row;
import org.apache.spark.sql.RowFactory;
import org.apache.spark.sql.SQLContext;
import org.apache.spark.sql.types.DataTypes;
import org.apache.spark.sql.types.MetadataBuilder;
import org.apache.spark.sql.types.StructField;
import org.apache.spark.sql.types.StructType;
import org.graphframes.GraphFrame;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public class Exercise_4 {
	
	public static void wikipedia(JavaSparkContext ctx, SQLContext sqlCtx) {
		java.util.List<Row> vertices_list = new ArrayList<Row>();

		ClassLoader classloader = Thread.currentThread().getContextClassLoader();

		try (InputStream is = classloader.getResourceAsStream("wiki-vertices.txt");
			 InputStreamReader streamReader = new InputStreamReader(is, StandardCharsets.UTF_8);
			 BufferedReader br = new BufferedReader(streamReader);) {
			String line;
			while ((line = br.readLine()) != null) {
				String[] parts = line.split("\t"); // split line into ID and title using tab as separator
				long id = Long.parseLong(parts[0]); // convert ID to long
				String title = parts[1];

				vertices_list.add(RowFactory.create(id, title));
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		JavaRDD<Row> vertices_rdd = ctx.parallelize(vertices_list);


		StructType vertices_schema = new StructType(new StructField[]{
				new StructField("id", DataTypes.LongType, true, new MetadataBuilder().build()),
				new StructField("name", DataTypes.StringType, true, new MetadataBuilder().build())
		});

		Dataset<Row> vertices =  sqlCtx.createDataFrame(vertices_rdd, vertices_schema);


		// Edges
		// edges creation
		java.util.List<Row> edges_list = new ArrayList<Row>();

		try (InputStream is = classloader.getResourceAsStream("wiki-edges.txt");
			 InputStreamReader streamReader = new InputStreamReader(is, StandardCharsets.UTF_8);
			 BufferedReader br = new BufferedReader(streamReader);) {
			String line;
			while ((line = br.readLine()) != null) {
				String[] parts = line.split("\t"); // split line into ID and title using tab as separator
				long src = Long.parseLong(parts[0]); // convert src to long
				long dst = Long.parseLong(parts[1]); // convert dst to long

				edges_list.add(RowFactory.create(src, dst));
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		JavaRDD<Row> edges_rdd = ctx.parallelize(edges_list);


		StructType edges_schema = new StructType(new StructField[]{
				new StructField("src", DataTypes.LongType, true, new MetadataBuilder().build()),
				new StructField("dst", DataTypes.LongType, true, new MetadataBuilder().build())
		});

		Dataset<Row> edges = sqlCtx.createDataFrame(edges_rdd, edges_schema);

		GraphFrame gf = GraphFrame.apply(vertices,edges);

		System.out.println(gf);

		gf.edges().show();
		gf.vertices().show();


		/*double[] resetProbs = {0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.30};

		for (double prob : resetProbs) {
			for (int i = 20; i > 0; i--) {
				GraphFrame pageRank = gf.pageRank().resetProbability(prob).maxIter(i).run();

				// Sort the results by PageRank score in descending order
				Dataset<Row> top10 = pageRank.vertices().orderBy(org.apache.spark.sql.functions.col("pagerank").desc()).limit(10);

				// Show the top 10 articles
				System.out.println("Reset Probability: " + (1-prob));
				System.out.println("Max Iterations: " + (i));
				top10.show(false);
			}
		}*/

	}
	
}
