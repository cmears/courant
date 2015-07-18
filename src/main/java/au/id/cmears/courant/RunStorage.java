package au.id.cmears.courant;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by chris on 9/07/15.
 */
public class RunStorage {
    public static void recordRun(Context context, Run run) {
        RunOpenHelper roh = new RunOpenHelper(context);
        SQLiteDatabase db = roh.getWritableDatabase();
        ContentValues cv = new ContentValues();
        cv.put("json", run.toJSON().toString());
        cv.put("uuid", run.uuidString());
        db.insert(RunOpenHelper.RUNS_TABLE_NAME, null, cv);
    }

    public static List<Run> getRuns(Context context) {
        RunOpenHelper roh = new RunOpenHelper(context);
        SQLiteDatabase db = roh.getReadableDatabase();
        String [] columns = {"json"};
        Cursor cursor = db.query(
                RunOpenHelper.RUNS_TABLE_NAME,
                columns,
                null,
                null,
                null,
                null,
                null,
                null);
        List<Run> list = new LinkedList<Run>();
        try {
            cursor.moveToFirst();
            while (!cursor.isAfterLast()) {
                int colIdx = cursor.getColumnIndex("json");
                String js = cursor.getString(colIdx);
                Log.d("RunStorage", js);
                JSONObject j = new JSONObject(js);
                Log.d("RunStorage", j.toString());
                Run run = new Run(j);
                Log.d("RunStorage", run.toJSON().toString());
                list.add(run);
                cursor.moveToNext();
            }
        } catch (JSONException e) {
            return null;
        }
        return list;
    }
}


class RunOpenHelper extends SQLiteOpenHelper {

    private static final int DATABASE_VERSION = 1;
    public static final String RUNS_TABLE_NAME = "runs";
    private static final String RUNS_TABLE_CREATE =
            "CREATE TABLE " + RUNS_TABLE_NAME + " (" +
                    "uuid" + " TEXT, " +
                    "json" + " TEXT);";

    RunOpenHelper(Context context) {
        super(context, "rundb", null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        db.execSQL(RUNS_TABLE_CREATE);
    }

    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {}
}