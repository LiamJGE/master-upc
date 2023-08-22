package exercise_3;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.tuple.Triple;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.graphx.*;
import org.apache.spark.sql.SQLContext;
import org.apache.spark.storage.StorageLevel;
import scala.Int;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.JavaConverters;
import scala.reflect.ClassTag$;
import scala.runtime.AbstractFunction1;
import scala.runtime.AbstractFunction2;
import scala.runtime.AbstractFunction3;


import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

import static com.google.common.primitives.Ints.min;
import static java.lang.Math.abs;


public class Exercise_3 {

    private static class VProg extends AbstractFunction3<Long,List<Long>,List<Long>,List<Long>> implements Serializable {
        @Override
        public List<Long> apply(Long vertexID, List<Long> vertexValue, List<Long> message) {
            if(abs(message.get(0))<abs(vertexValue.get(0))){
                message.add(vertexID);
                vertexValue=message;
            }

            return vertexValue;
        }
    }

    private static class sendMsg extends AbstractFunction1<EdgeTriplet<List<Long>,Integer>, Iterator<Tuple2<Object,List<Long>>>> implements Serializable {
        @Override
        public Iterator<Tuple2<Object, List<Long>>> apply(EdgeTriplet<List<Long>, Integer> triplet) {
            Tuple2<Object,List<Long>> sourceVertex = triplet.toTuple()._1();
            sourceVertex._2.set(0,sourceVertex._2.get(0)+triplet.attr);
            return JavaConverters.asScalaIteratorConverter(Arrays.asList(new Tuple2<Object,List<Long>>(triplet.dstId(),sourceVertex._2)).iterator()).asScala();
        }
    }

    private static class merge extends AbstractFunction2<List<Long>,List<Long>,List<Long>> implements Serializable {
        @Override
        public List<Long> apply(List<Long> o, List<Long> o2) {
            if (abs(o.get(0))<abs(o2.get(0))){
                return o;
            }else{
                return o2;
            }
        }
    }

    public static void shortestPathsExt(JavaSparkContext ctx) {
        Map<Long, String> labels = ImmutableMap.<Long, String>builder()
                .put(1l, "A")
                .put(2l, "B")
                .put(3l, "C")
                .put(4l, "D")
                .put(5l, "E")
                .put(6l, "F")
                .build();

        List<Tuple2<Object,List<Long>>> vertices = Lists.newArrayList(
                new Tuple2<Object,List<Long>>(1l,Lists.newArrayList(0L,1L)),
                new Tuple2<Object,List<Long>>(2l,Lists.newArrayList(Long.MAX_VALUE, 2L)),
                new Tuple2<Object,List<Long>>(3l,Lists.newArrayList(Long.MAX_VALUE,3L)),
                new Tuple2<Object,List<Long>>(4l,Lists.newArrayList(Long.MAX_VALUE,4L)),
                new Tuple2<Object,List<Long>>(5l,Lists.newArrayList(Long.MAX_VALUE,5L)),
                new Tuple2<Object,List<Long>>(6l,Lists.newArrayList(Long.MAX_VALUE,6L))
        );
        List<Edge<Integer>> edges = Lists.newArrayList(
                new Edge<Integer>(1l,2l, 4), // A --> B (4)
                new Edge<Integer>(1l,3l, 2), // A --> C (2)
                new Edge<Integer>(2l,3l, 5), // B --> C (5)
                new Edge<Integer>(2l,4l, 10), // B --> D (10)
                new Edge<Integer>(3l,5l, 3), // C --> E (3)
                new Edge<Integer>(5l, 4l, 4), // E --> D (4)
                new Edge<Integer>(4l, 6l, 11) // D --> F (11)
        );

        JavaRDD<Tuple2<Object,List<Long>>> verticesRDD = ctx.parallelize(vertices);
        JavaRDD<Edge<Integer>> edgesRDD = ctx.parallelize(edges);

        Graph<List<Long>,Integer> G = Graph.apply(verticesRDD.rdd(),edgesRDD.rdd(),(Lists.newArrayList(0L)), StorageLevel.MEMORY_ONLY(), StorageLevel.MEMORY_ONLY(),
                scala.reflect.ClassTag$.MODULE$.apply(List.class),scala.reflect.ClassTag$.MODULE$.apply(Integer.class));

        GraphOps ops = new GraphOps(G, scala.reflect.ClassTag$.MODULE$.apply(List.class),scala.reflect.ClassTag$.MODULE$.apply(Integer.class));
        ops.pregel(Lists.newArrayList(Long.MAX_VALUE),
                        Integer.MAX_VALUE,
                        EdgeDirection.Out(),
                        new VProg(),
                        new sendMsg(),
                        new merge(),
                        ClassTag$.MODULE$.apply(List.class))
                .vertices()
                .toJavaRDD()
                .foreach(v -> {
                    Tuple2<Object,List<Long>> vertex = (Tuple2<Object,List<Long>>)v;
                    Long cost=vertex._2.remove(0);
                    System.out.println("Minimum cost to get from " + labels.get(1l) + " to " + labels.get(vertex._1) + " is " + vertex._2.stream().map(labels::get).filter(Objects::nonNull).collect(Collectors.toList()) + " with cost " + cost);
                });
    }

}
