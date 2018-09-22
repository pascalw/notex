use chrono::prelude::*;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Notebook {
    pub id: i32,
    pub name: String,
    pub created_at: DateTime<Utc>,
    pub system_updated_at: DateTime<Utc>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct NewNotebook {
    pub name: String,
    pub created_at: DateTime<Utc>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct NotebookUpdate {
    pub name: String,
}

impl TypeIdentifiable for Notebook {
    fn type_name(&self) -> &'static str {
        "Notebook"
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Note {
    pub id: i32,
    pub title: String,
    pub tags: Vec<Tag>,
    pub notebook_id: i32,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub system_updated_at: DateTime<Utc>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct NewNote {
    pub title: String,
    pub tags: Vec<Tag>,
    pub created_at: DateTime<Utc>,
    pub notebook_id: i32,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct NoteUpdate {
    pub title: String,
    pub tags: Vec<Tag>,
    pub updated_at: DateTime<Utc>,
}

impl TypeIdentifiable for Note {
    fn type_name(&self) -> &'static str {
        "Note"
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "data")]
pub enum Content {
    Text { text: String },
    Code { language: String, code: String },
}

pub type Tag = String;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContentBlock {
    pub id: i32,
    pub content: Content,
    pub system_updated_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub note_id: i32,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct NewContentBlock {
    pub content: Content,
    pub created_at: DateTime<Utc>,
    pub note_id: i32,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ContentBlockUpdate {
    pub content: Content,
    pub updated_at: DateTime<Utc>,
}

impl TypeIdentifiable for ContentBlock {
    fn type_name(&self) -> &'static str {
        "ContentBlock"
    }
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Deletion {
    pub id: i32,
    #[serde(rename = "type")]
    pub type_: String,
    pub resource_id: i32,
    pub system_updated_at: DateTime<Utc>,
}

pub trait TypeIdentifiable {
    fn type_name(&self) -> &'static str;
}