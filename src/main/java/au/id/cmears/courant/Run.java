package au.id.cmears.courant;

import android.location.Location;
import android.util.Log;
import android.util.Pair;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.UUID;

public class Run {
    LinkedList<Segment> segments;
    UUID uuid;
    long startTime;

    public Run() {
        uuid = UUID.randomUUID();
        segments = new LinkedList<>();
        newSegment();
        startTime = System.currentTimeMillis();
    }

    public long startTime() { return startTime; }

    public void newLocation(Location loc) {
        segments.getLast().newLocation(loc);
    }

    public void newSegment() {
        segments.add(new Segment());
    }

    public Segment getLastSegment() {
        if (segments.size() == 0) return null;
        return segments.get(segments.size()-1);
    }

    public int numSamples() {
        int n = 0;
        for (Segment s : segments) {
            n += s.numSamples();
        }
        return n;
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            json.put("uuid", uuid.toString());
            JSONArray segmentArray = new JSONArray();
            for (Segment s : segments)
                segmentArray.put(s.toJSON());
            json.put("segments", segmentArray);
            return json;
        } catch (JSONException e) {
            Log.d("Run", e.toString());
            return null;
        }
    }

    public Run(JSONObject json) throws JSONException {
        String uuidS = json.getString("uuid");
        uuid = UUID.fromString(uuidS);
        JSONArray segmentArray = json.getJSONArray("segments");
        segments = new LinkedList<>();
        for (int i = 0; i < segmentArray.length(); i++) {
            segments.add(new Segment(segmentArray.getJSONObject(i)));
        }
    }

    public String uuidString() {
        return uuid.toString();
    }

    Pair<Double,Double> latestPosition() {
        Sample samp = getLastSample();
        if (samp == null) return null;
        return new Pair<>(samp.getLatitude(), samp.getLongitude());
    }

    public Float latestSpeed() {
        Sample samp = getLastSample();
        if (samp == null) return null;
        return samp.getSpeed();
    }

    public Float latestBearing() {
        Sample samp = getLastSample();
        if (samp == null) return null;
        return samp.getBearing();
    }

    public Float latestAccuracy() {
        Sample samp = getLastSample();
        if (samp == null) return null;
        return samp.getAccuracy();
    }

    public Float latestAverageSpeed() {
        Segment seg = segments.getLast();
        if (seg == null) return null;
        if (seg.samples.size() < 2) return null;
        Sample samp1 = seg.samples.get(seg.samples.size()-2);
        Sample samp2 = seg.samples.get(seg.samples.size()-1);
        float dist = samp1.distanceTo(samp2);
        float deltat = samp2.getTime() - samp1.getTime();
        float avgspeed = dist / deltat;
        // avgspeed is m/ms
        // convert to km/h
        return avgspeed * 3600;
    }

    public Sample getLastSample() {
        // Find the last *non-empty* segment.
        Segment seg = null;
        for (Segment s : segments) {
            if (s.samples.size() != 0)
                seg = s;
        }
        if (seg == null) return null;
        return seg.getLastSample();
    }

    public double totalDisplacement() {
        double accum = 0;
        for (Segment s : segments)
            accum += s.startFinishDisplacement();
        return accum;
    }

    public double totalDistance() {
        double accum = 0;
        for (Segment s : segments)
            accum += s.totalDistance();
        return accum;
    }
}

class Segment {
    ArrayList<Sample> samples;

    public Segment() {
        samples = new ArrayList<>();
    }

    public void newLocation(Location loc) {
        samples.add(new Sample(loc));
    }

    public int numSamples() {
        return samples.size();
    }

    public Sample getLastSample() {
        if (samples.size() == 0)
            return null;
        return samples.get(samples.size() - 1);
    }

    public double startFinishDisplacement() {
        if (samples.size() < 2)
            return 0;
        Sample s1 = samples.get(0);
        Sample s2 = samples.get(samples.size()-1);
        return s1.distanceTo(s2);
    }

    public double totalDistance() {
        double accum = 0;
        Sample prev = null;
        for (Sample s : samples) {
            if (prev != null) {
                accum += prev.distanceTo(s);
            }
            prev = s;
        }
        return accum;
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            JSONArray sampleArray = new JSONArray();
            for (Sample s : samples)
                sampleArray.put(s.toJSON());
            json.put("samples", sampleArray);
        } catch (Exception e) {
            Log.e("Segment", e.toString());
            return null;
        }
        return json;
    }

    public Segment(JSONObject json) throws JSONException {
        JSONArray sampleArray = json.getJSONArray("samples");
        samples = new ArrayList<>();
        for (int i = 0 ; i < sampleArray.length() ; i++) {
            samples.add(new Sample(sampleArray.getJSONObject(i)));
        }
    }
}

class Sample {
    private Location loc;

    public Sample(Location _loc) {
        loc = _loc;
    }

    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        try {
            json.put("latitude", loc.getLatitude());
            json.put("longitude", loc.getLongitude());
            if (loc.hasAltitude()) json.put("altitude", loc.getAltitude());
            if (loc.hasBearing()) json.put("bearing", loc.getBearing());
            if (loc.hasAccuracy()) json.put("accuracy", loc.getAccuracy());
            if (loc.hasSpeed()) json.put("speed", loc.getSpeed());
            json.put("time", loc.getTime());
            return json;
        } catch (Exception e) {
            Log.e("Sample", e.toString());
            return null;
        }
    }

    public Sample(JSONObject json) throws JSONException {
        Location l = new Location("gps");
        l.setLatitude(json.getDouble("latitude"));
        l.setLongitude(json.getDouble("longitude"));
        l.setTime(json.getInt("time"));
        if (json.has("altitude")) l.setAltitude(json.getDouble("altitude"));
        if (json.has("bearing")) l.setBearing((float) json.getDouble("bearing"));
        if (json.has("accuracy")) l.setAccuracy((float) json.getDouble("accuracy"));
        if (json.has("speed")) l.setSpeed((float)json.getDouble("speed"));
        loc = l;
    }

    public Float getBearing() {
        if (loc.hasBearing()) return loc.getBearing();
        else return null;
    }

    public Float getSpeed() {
        if (loc.hasSpeed()) return 3.6f * loc.getSpeed();
        else return null;
    }

    public Float getAccuracy() {
        if (loc.hasAccuracy()) return loc.getAccuracy();
        else return null;
    }

    public double getLatitude() {
        return loc.getLatitude();
    }

    public double getLongitude() {
        return loc.getLongitude();
    }

    public float distanceTo(Sample otherSample) {
        return loc.distanceTo(otherSample.loc);
    }

    public long getTime() {
        return loc.getTime();
    }
}
